import os
import subprocess
import pandas as pd

def run_bedtools_intersect(query_file, database_file, output_file):
    command = f"bedtools intersect -a {query_file} -b {database_file} -wa -wb -f 0.5 > {output_file}"
    subprocess.run(command, shell=True, check=True)

def calculate_overlap(intersect_file, query_file):
    try:
        df = pd.read_csv(intersect_file, sep='\t', header=None)
        total_query_regions = len(pd.read_csv(query_file, sep='\t', header=None))
        if df.empty:
            return 0, total_query_regions, 0
        overlapping_regions = df[3].nunique()
        percent_overlap = (overlapping_regions / total_query_regions) * 100
        return overlapping_regions, total_query_regions, percent_overlap
    except pd.errors.EmptyDataError:
        total_query_regions = len(pd.read_csv(query_file, sep='\t', header=None))
        return 0, total_query_regions, 0

def process_tf_folder(query_file, tf_folder, tf_name):
    results = []
    for file in os.listdir(tf_folder):
        if file.endswith('.bed'):
            database_file = os.path.join(tf_folder, file)
            output_file = f"{os.path.basename(query_file)}_{tf_name}_{file}_intersect_result.txt"
            run_bedtools_intersect(query_file, database_file, output_file)
            
            overlapping, total, percent = calculate_overlap(output_file, query_file)
            results.append({
                'Query': os.path.basename(query_file),
                'Database': file,
                'Overlapping Regions': overlapping,
                'Total Query Regions': total,
                'Percent Overlap': percent
            })
            
            os.remove(output_file)  # Clean up temporary file
    
    return pd.DataFrame(results)

# Set the paths
query_folders = ["1hr_antagonized", "1hr_unaffected", "24hr_antagonized", "24hr_unaffected", "6hr_antagonized", "6hr_unaffected"]
comparison_dir = "."  # Current directory

# Specify the transcription factors to include in the analysis
tfs_to_analyze = []

# Process each query folder
for query_folder in query_folders:
    # Get all .bed files in the query folder that match the specified TFs
    query_files = [f for f in os.listdir(query_folder) if f.endswith('.filtered.bed') and f.split('.')[0] in tfs_to_analyze]
    
    for query_file in query_files:
        # Extract TF name from the query file (e.g., 'PAX5' from 'PAX5.filtered.bed')
        tf_name = query_file.split('.')[0]
        
        query_path = os.path.join(query_folder, query_file)
        tf_folder = os.path.join(comparison_dir, tf_name)
        
        if os.path.exists(tf_folder):
            results_df = process_tf_folder(query_path, tf_folder, tf_name)
            print(f"\nResults for {query_folder}/{query_file} vs {tf_name}:")
            if results_df.empty:
                print("No overlaps found for any database files.")
            else:
                print(results_df.to_string(index=False))
            
            # Optionally, save results to a CSV file
            results_df.to_csv(f"{query_folder}_{query_file}_{tf_name}_overlap_results.csv", index=False)
        else:
            print(f"Folder for {tf_name} not found in comparison directory")

print("\nOverlap analysis completed.")
