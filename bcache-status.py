#!/usr/bin/env python3
#
# Dumb script to dump (some) of bcache status
# Copyright 2014 Darrick J. Wong. All rights reserved.
#
# This file is part of Bcache.  Bcache is free software: you can
# redistribute it and/or modify it under the terms of the GNU General Public
# License as published by the Free Software Foundation, version 2.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
#


import os
import os.path
import sys
import re
import errno
import argparse


MAX_KEY_LENGTH		= 28
DEV_BLOCK_PATH		= '/dev/block/'
SYSFS_BCACHE_PATH	= '/sys/fs/bcache/'
SYSFS_BLOCK_PATH	= '/sys/block/'

UUID_LEN                = 36    # 8-4-4-4-12, 32 chars + 4 '-'s


def _file_to_lines(fname):
    'read content of sysfs attribute file to lines'
    try:
        with open(fname, 'r') as fd:
            return fd.readlines()
    except IOError as e:
        if (e[0] == errno.EPERM):
            print('Permission denied: root privilege is required!', file=sys.stderr)
            sys.exit(1)
    except:
        return []

def _file_to_line(fname):
    'read single line of sysfs attribute file'
    lines = _file_to_lines(fname)
    if lines:
        return lines[0].strip()
    return ''


def str_to_bool(x):
	return x == '1'

def format_sectors(x):
	'''Pretty print a sector count.'''
	sectors = float(x)
	asectors = abs(sectors)

	if asectors < 2:
		return '%d B' % (sectors * 512)
	elif asectors < 2048:
		return '%.2f KiB' % (sectors / 2)
	elif asectors < 2097152:
		return '%.1f MiB' % (sectors / 2048)
	elif asectors < 2147483648:
		return '%.0f GiB' % (sectors / 2097152)
	else:
		return '%.0f TiB' % (sectors / 2147483648)

def interpret_sectors(x):
	'''Interpret a pretty-printed disk size.'''
	factors = {
		'k': 1 << 10,
		'M': 1 << 20,
		'G': 1 << 30,
		'T': 1 << 40,
		'P': 1 << 50,
		'E': 1 << 60,
		'Z': 1 << 70,
		'Y': 1 << 80,
	}

	factor = 1
	if x[-1] in factors:
		factor = factors[x[-1]]
		x = x[:-1]
	return int(float(x) * factor / 512)

def pretty_size(x):
	return format_sectors(interpret_sectors(x))

def device_path(x):
	if not os.path.isdir(DEV_BLOCK_PATH):
		return '?'
	x = '%s/%s' % (DEV_BLOCK_PATH, x)
	return os.path.abspath(os.path.join(os.path.dirname(x), os.readlink(x)))

def str_device_path(x):
	return '%s (%s)' % (device_path(x), x)

def dump_bdev(bdev_path):
	'''Dump a backing device stats.'''
	global MAX_KEY_LENGTH
	attrs = [
		('../dev',		'Device File',		str_device_path),
		('dev/dev',		'bcache Device File',	str_device_path),
		('../size',		'Size',			format_sectors),
		('cache_mode',		'Cache Mode',		None),
		('readahead',		'Readahead',		None),
		('sequential_cutoff',	'Sequential Cutoff',	pretty_size),
		('sequential_merge',	'Merge sequential?',	str_to_bool),
		('state',		'State',		None),
		('writeback_running',	'Writeback?',		str_to_bool),
		('dirty_data',		'Dirty Data',		pretty_size),
	]

	print('--- Backing Device ---')
	for (sysfs_name, display_name, conversion_func) in attrs:
		val = file_to_line('%s/%s' % (bdev_path, sysfs_name))
		if conversion_func is not None:
			val = conversion_func(val)
		if display_name is None:
			display_name = sysfs_name
		print('  %-*s%s' % (MAX_KEY_LENGTH - 2, display_name, val))

def dump_cachedev(cachedev_path):
	'''Dump a cachding device stats.'''
	def fmt_cachesize(val):
		return '%s\t(%.0f%%)' % (format_sectors(val), float(val) / cache_size * 100)

	global MAX_KEY_LENGTH
	attrs = [
		('../dev',			'Device File',		str_device_path),
		('../size',			'Size',			format_sectors),
		('block_size',			'Block Size',		pretty_size),
		('bucket_size',			'Bucket Size',		pretty_size),
		('cache_replacement_policy',	'Replacement Policy',	None),
		('discard',			'Discard?',		str_to_bool),
		('io_errors',			'I/O Errors',		None),
		('metadata_written',		'Metadata Written',	pretty_size),
		('written',			'Data Written',		pretty_size),
		('nbuckets',			'Buckets',		None),
		(None,				'Cache Used',		lambda x: fmt_cachesize(used_sectors)),
		(None,				'Cache Unused',		lambda x: fmt_cachesize(unused_sectors)),
	]

	stats = get_cache_priority_stats(cachedev_path)
	cache_size = int(file_to_line('%s/../size' % cachedev_path))
	unused_sectors = float(stats['Unused'][:-1]) * cache_size / 100
	used_sectors = cache_size - unused_sectors

	print('--- Cache Device ---')
	for (sysfs_name, display_name, conversion_func) in attrs:
		if sysfs_name is not None:
			val = file_to_line('%s/%s' % (cachedev_path, sysfs_name))
		if conversion_func is not None:
			val = conversion_func(val)
		if display_name is None:
			display_name = sysfs_name
		print('  %-*s%s' % (MAX_KEY_LENGTH - 2, display_name, val))

def hits_to_str(hits_str, misses_str):
	'''Render a hits/misses ratio as a string.'''
	hits = int(hits_str)
	misses = int(misses_str)

	ret = '%d' % hits
	if hits + misses != 0:
		ret = '%s\t(%.d%%)' % (ret, 100 * hits / (hits + misses))
	return ret

def dump_stats(sysfs_path, indent_str, stats):
	'''Dump stats on a bcache device.'''
	stat_types = [
		('five_minute',	'Last 5min'),
		('hour',	'Last Hour'),
		('day',		'Last Day'),
		('total',	'Total'),
	]
	attrs = ['bypassed', 'cache_bypass_hits', 'cache_bypass_misses', 'cache_hits', 'cache_misses']
	display = [
		('Hits',		lambda: hits_to_str(stat_data['cache_hits'], stat_data['cache_misses'])),
		('Misses',		lambda: stat_data['cache_misses']),
		('Bypass Hits',		lambda: hits_to_str(stat_data['cache_bypass_hits'], stat_data['cache_bypass_misses'])),
		('Bypass Misses',	lambda: stat_data['cache_bypass_misses']),
		('Bypassed',		lambda: pretty_size(stat_data['bypassed'])),
	]

	for (sysfs_name, stat_display_name) in stat_types:
		if len(stats) > 0 and sysfs_name not in stats:
			continue
		stat_data = {}
		for attr in attrs:
			val = _file_to_line('%s/stats_%s/%s' % (sysfs_path, sysfs_name, attr))
			stat_data[attr] = val
		for (display_name, str_func) in display:
			d = '%s%s %s' % (indent_str, stat_display_name, display_name)
			print('%-*s%s' % (MAX_KEY_LENGTH, d, str_func()))

def get_cache_priority_stats(cache):
	'''Retrieve priority stats from a cache.'''
	attrs = {}

	for line in _file_to_lines('%s/priority_stats' % cache):
		x = line.split()
		key = x[0]
		value = x[1]
		attrs[key[:-1]] = value
	return attrs

def dump_bcache(bcache_sysfs_path, stats, print_subdevices, device):
	'''Dump bcache stats'''
	def fmt_cachesize(val):
		return '%s\t(%.0f%%)' % (format_sectors(val), 100.0 * val / cache_sectors)

	attrs = [
		(None,					'UUID',			lambda x: os.path.basename(bcache_sysfs_path)),
		('block_size',				'Block Size',		pretty_size),
		('bucket_size',				'Bucket Size',		pretty_size),
		('congested', 				'Congested?',		str_to_bool),
		('congested_read_threshold_us',		'Read Congestion',	lambda x: '%.1fms' % (int(x) / 1000)),
		('congested_write_threshold_us',	'Write Congestion',	lambda x: '%.1fms' % (int(x) / 1000)),
		(None,					'Total Cache Size',	lambda x: format_sectors(cache_sectors)),
		(None,					'Total Cache Used',	lambda x: fmt_cachesize(cache_used_sectors)),
		(None,					'Total Cache Unused',	lambda x: fmt_cachesize(cache_unused_sectors)),
		#('dirty_data',				'Dirty Data',		lambda x: fmt_cachesize(interpret_sectors(x))),		# disappeared in 3.13?
		('cache_available_percent',		'Evictable Cache',	lambda x: '%s\t(%s%%)' % (format_sectors(float(x) * cache_sectors / 100), x)),
		(None,					'Replacement Policy',	lambda x: replacement_policies.pop() if len(replacement_policies) == 1 else '(Various)'),
		(None,					'Cache Mode',		lambda x: cache_modes.pop() if len(cache_modes) == 1 else '(Various)'),
	]

	# Calculate aggregate data
	cache_sectors = 0
	cache_unused_sectors = 0
	cache_modes = set()
	replacement_policies = set()
	for obj in os.listdir(bcache_sysfs_path):
		if not os.path.isdir('%s/%s' % (bcache_sysfs_path, obj)):
			continue
		if obj.startswith('cache'):
			cache_size = int(_file_to_line('%s/%s/../size' % (bcache_sysfs_path, obj)))
			cache_sectors += cache_size
			cstats = get_cache_priority_stats('%s/%s' % (bcache_sysfs_path, obj))
			unused_size = float(cstats['Unused'][:-1]) * cache_size / 100
			cache_unused_sectors += unused_size
			replacement_policies.add(_file_to_line('%s/%s/cache_replacement_policy' % (bcache_sysfs_path, obj)))
		elif obj.startswith('bdev'):
			cache_modes.add(_file_to_line('%s/%s/cache_mode' % (bcache_sysfs_path, obj)))
	cache_used_sectors = cache_sectors - cache_unused_sectors

	# Dump basic stats
	print("--- bcache ---")
	for (sysfs_name, display_name, conversion_func) in attrs:
		if sysfs_name is not None:
			val = _file_to_line('%s/%s' % (bcache_sysfs_path, sysfs_name))
		else:
			val = None
		if conversion_func is not None:
			val = conversion_func(val)
		if display_name is None:
			display_name = sysfs_name
		print('%-*s%s' % (MAX_KEY_LENGTH, display_name, val))
	dump_stats(bcache_sysfs_path, '', stats)

	# Dump sub-device stats
	if not print_subdevices:
		return
	for obj in os.listdir(bcache_sysfs_path):
		if not os.path.isdir('%s/%s' % (bcache_sysfs_path, obj)):
			continue
		if obj.startswith('bdev'):
			dump_bdev('%s/%s' % (bcache_sysfs_path, obj))
			dump_stats('%s/%s' % (bcache_sysfs_path, obj), '  ', stats)
		elif obj.startswith('cache'):
			dump_cachedev('%s/%s' % (bcache_sysfs_path, obj))

def map_uuid_to_device():
    '''Map bcache UUIDs to device files.'''
    ret = {}

    if not os.path.isdir(SYSFS_BLOCK_PATH):
        return ret
    for bdev in os.listdir(SYSFS_BLOCK_PATH):
        link = '%s%s/bcache/cache' % (SYSFS_BLOCK_PATH, bdev)
        if not os.path.islink(link):
            continue
        basename = os.path.basename(os.readlink(link))
        ret[basename] = _file_to_line('%s%s/dev' % (SYSFS_BLOCK_PATH, bdev))
    return ret


def scan_backing_devs():
    '''
    recognize backing devices "bcacheN" in the system

    return list of tuples like ('bcacheN', 'sdX', 'CSET-UUID' or 'uncached')
    '''
    
    backing_devs = []
    p_bdi = re.compile(r'/block/([\w|\W|\/]+)/bcache')

    for bdev in os.listdir(SYSFS_BLOCK_PATH):
        if bdev.startswith('bcache'):
            path = '%s%s/bcache' % (SYSFS_BLOCK_PATH, bdev)
            cs_path = '%s/cache' % path
            real = os.path.realpath(path)
            m = p_bdi.search(real)
            if os.path.exists(cs_path):
                backing_devs.append(
                        (
                            bdev,
                            m.group(1),
                            os.path.basename(os.path.realpath(cs_path))
                        )
                    )
            else:
                backing_devs.append((bdev, m.group(1), 'uncached'))

    return backing_devs


def scan_cache_sets():
    '''
    recognize cache_set(s) in the system

    return list of tuples ('CSET-UUID', 'cache device')
    '''

    cache_sets = []
    p_cache = re.compile(r'/block/([\w|\W|\/]+)/bcache')

    for bdev in os.listdir(SYSFS_BCACHE_PATH):
        if len(bdev) == UUID_LEN:
            # one cache_set contains one cache device only currently
            path = '%s/%s/cache0' % (SYSFS_BCACHE_PATH, bdev)
            real = os.path.realpath(path)
            m = p_cache.search(real)
            cache_sets.append((bdev, m.group(1)))

    return cache_sets


def bcache_topology(cache_sets, backing_devs):
    '''
    construct map of cache_set <-> backing devs
    @cache_sets     : list of tuples ('CSET-UUID', 'cache device')
    @backing_devs   : list of tuples like ('bcacheN', 'sdX', 'CSET-UUID' or 'uncached')

    return dict of lists like
        'CSET-UUID' : ['cache dev name', 'backing dev name : bcacheN', ...]
        'uncached' : ['backing dev name : bcacheN', ...]
    '''

    topology = {}
    
    for cset_uuid, cache_dev in cache_sets:
        topology[cset_uuid] = [cache_dev]
        for item in backing_devs:
            if item[2] == cset_uuid:
                topology[cset_uuid].append('%s : %s' % (item[1], item[0]))

    topology['uncached'] = []
    for item in backing_devs:
        if item[2] == 'uncached':
            topology['uncached'].append('%s : %s' % (item[1], item[0]))
    if len(topology['uncached']) == 0:
        del topology['uncached']

    return topology


def dump_topology(topology):
    '''
    Print bcache topology in the system

    @topology: dict of lists like
                    'CSET-UUID' : ['cache dev name', 'backing dev name : bcacheN', ...]
                    'uncached' : ['backing dev name : bcacheN', ...]
    '''

    # %36s : %16s
    print('%-60s  backing dev(s)' % 'cache_set')
    print('-'*60 + '  ' + '-'*16)
    for cset_uuid in topology:
        item = topology[cset_uuid]
        if cset_uuid != 'uncached':
            print('%-36s : %-20s  ' % (cset_uuid, item[0]), end='')
            if len(item) == 1:
                print(' (None)')
            else:
                for i in item[1:-2]:
                    print(' %s ,' % i, end='')
                print(' %s' % item[-1])

    if 'uncached' in topology:
        item = topology['uncached']
        print('%-60s ' % 'uncached', end='')
        for i in item[1:-2]:
             print(' %s ,' % i, end='')
        print(' %s' % item[-1])


def bcache_module_loaded():
    if not os.path.isdir(SYSFS_BCACHE_PATH):
    	return False
    return True


def args_parser():
    'construct argparser to parse command line'

    parser = argparse.ArgumentParser(add_help=False)

    parser.add_argument('-h', '--help',
                        help='Show this help message and exit',
                        action='store_true')

    parser.add_argument('--five-minute',
                        help='Print the last five minutes of stats.',
                        action='store_true')
    parser.add_argument('--hour',
                        help='Print the last hour of stats.',
                        action='store_true')
    parser.add_argument('--day',
                        help='Print the last day of stats.',
                        action='store_true')
    parser.add_argument('--total',
                        help='Print total stats.',
                        action='store_true')
    parser.add_argument('--all',
                        help='Print all stats.',
                        action='store_true')
    parser.add_argument('--reset-stats',
                        help='Reset stats after printing them.',
                        action='store_true')

    parser.add_argument('--sub-status',
                        help='Print subdevice status.',
                        action='store_true')

    parser.add_argument('--gc',
                        help='Invoke GC before printing status.',
                        action='store_true')

    return parser


def stats_options(args):
    '''
    @args   : argparse.ArgumentParser().parse_args()
    return  : set of selected options about stats
    '''
    options = set()

    if args.five_minute:
    	options.add('five_minute')
    if args.hour:
    	options.add('hour')
    if args.day:
    	options.add('day')
    if args.total:
    	options.add('total')
    if args.all:
    	options.add('five_minute')
    	options.add('hour')
    	options.add('day')
    	options.add('total')

    if not options:
    	options.add('total')

    return options


def main():
    'entry point'

    global uuid_map
    stats = set()
    reset_stats = False
    print_subdevices = False
    run_gc = False

    parser = args_parser()
    args = parser.parse_args()
    if args.help:
    	parser.print_help()
    	exit(0)

    if not bcache_module_loaded():
        print('Module bcache is not loaded.')
        exit(0)

    stats = stats_options(args)

    if args.reset_stats:
    	reset_stats = True
    if args.sub_status:
    	print_subdevices = True
    if args.gc:
    	run_gc = True


    # list of tuples like ('bcacheN', 'sdX', 'CSET-UUID' or 'uncached')
    backing_devs = scan_backing_devs()
    # list of tuples ('CSET-UUID', 'cache device')
    cache_sets = scan_cache_sets()
    print(backing_devs)
    print(cache_sets)
    # dict of lists like
    #       'CSET-UUID' : ['cache dev name', 'backing dev name : bcacheN', ...]
    #       'uncached' : ['backing dev name : bcacheN', ...]
    topology = bcache_topology(cache_sets, backing_devs)
    dump_topology(topology)

    uuid_map = map_uuid_to_device()
    for cache in os.listdir(SYSFS_BCACHE_PATH):
    	if not os.path.isdir('%s%s' % (SYSFS_BCACHE_PATH, cache)):
    		continue

    	if run_gc:
    		with open('%s%s/internal/trigger_gc' % (SYSFS_BCACHE_PATH, cache), 'w') as fd:
    			fd.write('1\n')

    	dump_bcache('%s%s' % (SYSFS_BCACHE_PATH, cache), stats, print_subdevices, uuid_map.get(cache, '?'))

    	if reset_stats:
	    	with open('%s%s/clear_stats' % (SYSFS_BCACHE_PATH, cache), 'w') as fd:
	    		fd.write('1\n')

if __name__ == '__main__':
    main()
