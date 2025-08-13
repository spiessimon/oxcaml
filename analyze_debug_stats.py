#!/usr/bin/env python3
"""
Analyze DWARF debug stats JSON files and produce aggregated statistics.
"""

import json
import glob
import os
from collections import defaultdict

class StatEntry:
    def __init__(self, type_name, initial_size_memory, reduced_size_memory, evaluated_size_memory,
                 initial_size, reduced_size, evaluated_size, reduction_steps, 
                 evaluation_steps, dwarf_die_size, file_path):
        self.type_name = type_name
        self.initial_size_memory = initial_size_memory
        self.reduced_size_memory = reduced_size_memory
        self.evaluated_size_memory = evaluated_size_memory
        self.initial_size = initial_size
        self.reduced_size = reduced_size
        self.evaluated_size = evaluated_size
        self.reduction_steps = reduction_steps
        self.evaluation_steps = evaluation_steps
        self.dwarf_die_size = dwarf_die_size
        self.file_path = file_path

def parse_json_file(file_path):
    """Parse a single JSON file and return (entries, file_metadata) tuple."""
    entries = []
    file_metadata = {'sourcefile': 'unknown', 'cms_files_loaded': 0, 'cms_files_cached': 0, 'compilation_parameters': None}
    try:
        with open(file_path, 'r') as f:
            data = json.load(f)
            
        # Extract per-file metadata
        file_metadata['sourcefile'] = data.get('sourcefile', 'unknown')
        file_metadata['cms_files_loaded'] = data.get('cms_files_loaded', 0)
        file_metadata['cms_files_cached'] = data.get('cms_files_cached', 0)
        file_metadata['compilation_parameters'] = data.get('compilation_parameters', None)
        
        # Process variables
        for var_data in data.get('variables', []):
            try:
                entry = StatEntry(
                    type_name=var_data['type'],
                    initial_size_memory=int(var_data['initial_size_memory']),
                    reduced_size_memory=int(var_data['reduced_size_memory']),
                    evaluated_size_memory=int(var_data['evaluated_size_memory']),
                    initial_size=int(var_data['initial_size']),
                    reduced_size=int(var_data['reduced_size']),
                    evaluated_size=int(var_data['evaluated_size']),
                    reduction_steps=int(var_data['reduction_steps']),
                    evaluation_steps=int(var_data['evaluation_steps']),
                    dwarf_die_size=int(var_data['dwarf_die_size']),
                    file_path=file_path
                )
                entries.append(entry)
            except (ValueError, KeyError) as e:
                print("Warning: Skipping malformed variable in {}: {}".format(file_path, e))
                
    except (json.JSONDecodeError, IOError) as e:
        print("Warning: Could not parse {}: {}".format(file_path, e))
        
    return entries, file_metadata

def create_histogram_text(values, title, num_buckets=10):
    """Create a text-based histogram with integer buckets."""
    if not values:
        return "{}: No data\n".format(title)
    
    min_val = min(values)
    max_val = max(values)
    
    if min_val == max_val:
        return "{}: All values are {}\n".format(title, min_val)
    
    # Calculate integer bucket size, ensuring we cover the full range
    range_size = max_val - min_val
    bucket_size = max(1, (range_size + num_buckets - 1) // num_buckets)  # Round up division
    
    buckets = [0] * num_buckets
    bucket_ranges = []
    
    for i in range(num_buckets):
        start = min_val + i * bucket_size
        end = min_val + (i + 1) * bucket_size
        bucket_ranges.append((start, end))
    
    # Assign values to buckets
    for val in values:
        bucket_idx = min((val - min_val) // bucket_size, num_buckets - 1)
        buckets[bucket_idx] += 1
    
    # Create text histogram
    result = "{} (Range: {}-{}):\n".format(title, min_val, max_val)
    max_count = max(buckets)
    scale = 50.0 / max_count if max_count > 0 else 1
    
    for i, count in enumerate(buckets):
        start, end = bucket_ranges[i]
        bar_length = int(count * scale)
        bar = '#' * bar_length
        # Show inclusive start, exclusive end for integer ranges with comma formatting
        result += "[{:>8,}-{:>8,}): {:>6,} {}\n".format(start, end, count, bar)
    
    return result + "\n"

def analyze_stats():
    """Main analysis function."""
    # Find all JSON files
    json_files = glob.glob("**/*.debug-stats.json", recursive=True)
    
    if not json_files:
        print("No .debug-stats.json files found.")
        return
    
    # Parse all files
    all_entries = []
    all_file_metadata = []
    file_count = 0
    for json_file in json_files:
        entries, file_metadata = parse_json_file(json_file)
        if entries:
            all_entries.extend(entries)
            all_file_metadata.append((json_file, file_metadata))
            file_count += 1
    
    if not all_entries:
        print("No valid entries found.")
        return
    
    # Calculate total DWARF DIEs
    total_dwarf_dies = sum(e.dwarf_die_size for e in all_entries)
    
    # CMS file statistics for summary
    cms_loaded_counts = [metadata['cms_files_loaded'] for _, metadata in all_file_metadata]
    cms_cached_counts = [metadata['cms_files_cached'] for _, metadata in all_file_metadata]
    total_cms_loaded = sum(cms_loaded_counts)
    total_cms_cached = sum(cms_cached_counts)
    files_with_cms_loaded = sum(1 for count in cms_loaded_counts if count > 0)
    
    # Print summary header
    print("# DWARF Debug Statistics Analysis")
    print()
    print("**Summary:** {:,} files, {:,} variables, {:,} total DIEs".format(file_count, len(all_entries), total_dwarf_dies))
    print("**CMS Files:** {:,} loaded across {:,} files, {:,} cache hits".format(total_cms_loaded, files_with_cms_loaded, total_cms_cached))
    
    # Print compilation parameters - fail if not consistent
    compilation_params_sets = set()
    for _, metadata in all_file_metadata:
        params = metadata.get('compilation_parameters')
        if params:
            # Convert to a frozenset of items for hashability
            params_tuple = tuple(sorted(params.items()))
            compilation_params_sets.add(params_tuple)
    
    if not compilation_params_sets:
        print("**Error:** No compilation parameters found in JSON files.")
        return
    elif len(compilation_params_sets) > 1:
        print("**Error:** Files were compiled with different parameters. All files must use the same configuration.")
        return
    else:
        # All files used the same parameters
        params_dict = dict(list(compilation_params_sets)[0])
        config_parts = []
        for key, value in sorted(params_dict.items()):
            # Shorten parameter names for display
            short_name = key.replace('gdwarf_config_', '').replace('_', '-')
            config_parts.append("{}={}".format(short_name, value))
        print("**Configuration:** {}".format(", ".join(config_parts)))
    print()
    
    print("## Individual Variable Statistics")
    print()
    
    # Aggregate statistics for individual variables
    initial_sizes = [e.initial_size for e in all_entries]
    initial_sizes_memory = [e.initial_size_memory for e in all_entries]
    reduced_sizes = [e.reduced_size for e in all_entries]
    reduced_sizes_memory = [e.reduced_size_memory for e in all_entries]
    evaluated_sizes = [e.evaluated_size for e in all_entries]
    evaluated_sizes_memory = [e.evaluated_size_memory for e in all_entries]
    reduction_steps = [e.reduction_steps for e in all_entries]
    evaluation_steps = [e.evaluation_steps for e in all_entries]
    dwarf_die_sizes = [e.dwarf_die_size for e in all_entries]
    
    types = [e.type_name for e in all_entries]
    
    # Top values analysis function
    def print_top_values(values, labels, title, n=5):
        """Print top N values with their labels, deduplicated by (value, label) pairs."""
        if not values:
            return
            
        # Create a set to deduplicate (value, label) pairs
        unique_pairs = set(zip(values, labels))
        
        # Sort by value descending
        sorted_pairs = sorted(unique_pairs, key=lambda x: x[0], reverse=True)
        
        print("#### Top {} {}".format(n, title))
        print()
        for i, (value, label) in enumerate(sorted_pairs[:n]):
            print("{:2d}. **{:,}** - `{}`".format(i+1, value, label))
        print()
    
    # Initial Sizes section (non-memory first, then memory)
    print("### Initial Sizes")
    print(create_histogram_text(initial_sizes, "Initial Sizes"), end="")
    print_top_values(initial_sizes, types, "Initial Sizes")
    
    print("### Initial Sizes (Memory)")
    print(create_histogram_text(initial_sizes_memory, "Initial Sizes (Memory)"), end="")
    print_top_values(initial_sizes_memory, types, "Initial Sizes (Memory)")
    
    # Reduced Sizes section (non-memory first, then memory)
    print("### Reduced Sizes")
    print(create_histogram_text(reduced_sizes, "Reduced Sizes"), end="") 
    print_top_values(reduced_sizes, types, "Reduced Sizes")
    
    print("### Reduced Sizes (Memory)")
    print(create_histogram_text(reduced_sizes_memory, "Reduced Sizes (Memory)"), end="") 
    print_top_values(reduced_sizes_memory, types, "Reduced Sizes (Memory)")
    
    # Evaluated Sizes section (non-memory first, then memory)
    print("### Evaluated Sizes")
    print(create_histogram_text(evaluated_sizes, "Evaluated Sizes"), end="")
    print_top_values(evaluated_sizes, types, "Evaluated Sizes")
    
    print("### Evaluated Sizes (Memory)")
    print(create_histogram_text(evaluated_sizes_memory, "Evaluated Sizes (Memory)"), end="")
    print_top_values(evaluated_sizes_memory, types, "Evaluated Sizes (Memory)")
    
    # Reduction Steps section
    print("### Reduction Steps")
    print(create_histogram_text(reduction_steps, "Reduction Steps"), end="")
    print_top_values(reduction_steps, types, "Reduction Steps")
    
    # Evaluation Steps section
    print("### Evaluation Steps")
    print(create_histogram_text(evaluation_steps, "Evaluation Steps"), end="")
    print_top_values(evaluation_steps, types, "Evaluation Steps")
    
    # DWARF DIE Sizes section
    print("### DWARF DIE Sizes")
    print(create_histogram_text(dwarf_die_sizes, "DWARF DIE Sizes"), end="")
    print_top_values(dwarf_die_sizes, types, "DWARF DIE Sizes")
    
    # File-level aggregation and statistics
    file_stats = defaultdict(lambda: {'count': 0, 'total_initial_memory': 0, 'total_reduced_memory': 0, 'total_initial': 0, 'total_reduced': 0, 'total_dwarf_dies': 0, 'cms_files_loaded': 0, 'cms_files_cached': 0})
    
    for entry in all_entries:
        basename = os.path.basename(entry.file_path)
        file_stats[basename]['count'] += 1
        file_stats[basename]['total_initial_memory'] += entry.initial_size_memory
        file_stats[basename]['total_reduced_memory'] += entry.reduced_size_memory
        file_stats[basename]['total_initial'] += entry.initial_size
        file_stats[basename]['total_reduced'] += entry.reduced_size
        file_stats[basename]['total_dwarf_dies'] += entry.dwarf_die_size
    
    # Add CMS file counts to file stats
    for file_path, file_metadata in all_file_metadata:
        basename = os.path.basename(file_path)
        file_stats[basename]['cms_files_loaded'] = file_metadata['cms_files_loaded']
        file_stats[basename]['cms_files_cached'] = file_metadata['cms_files_cached']
    
    print("## File-level Statistics")
    print()
    print("Top 20 files by DWARF DIE size:")
    print()
    print("| File | Variables | Memory Initial | Memory Reduced | Size Initial | Size Reduced | Total DIEs | CMS Loaded |")
    print("|------|-----------|----------------|----------------|--------------|--------------|------------|------------|")
    
    # Sort by total DIEs descending
    sorted_files = sorted(file_stats.items(), 
                         key=lambda x: x[1]['total_dwarf_dies'], reverse=True)
    
    for filename, stats in sorted_files[:20]:  # Show top 20 files
        # Extract original source filename from .debug-stats.json
        if filename.endswith('.debug-stats.json'):
            source_filename = filename[:-len('.debug-stats.json')]
        else:
            source_filename = filename
        
        print("| {} | {:,} | {:,} | {:,} | {:,} | {:,} | {:,} | {:,} |".format(
            source_filename, stats['count'], stats['total_initial_memory'], 
            stats['total_reduced_memory'], stats['total_initial'], stats['total_reduced'], 
            stats['total_dwarf_dies'], stats['cms_files_loaded']))

if __name__ == "__main__":
    analyze_stats()