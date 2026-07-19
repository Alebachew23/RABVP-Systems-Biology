import pandas as pd
import re
import os

def sanitize_filename(filename):
    # Replace invalid characters with underscores
    return re.sub(r'[\\/*?:"<>|]', "_", filename)

def filter_bed_by_multiple_tfs(input_file, tf_dict):
    # Read the BED file
    df = pd.read_csv(input_file, sep='\t', header=None, 
                     names=['chr', 'start', 'end', 'name', 'score', 'strand', 
                            'thickStart', 'thickEnd', 'itemRgb'])
    
    # Filter and save for each TF
    for tf, ids in tf_dict.items():
        # Create a pattern that matches the TF name or any of its IDs
        pattern = '|'.join([re.escape(tf)] + [re.escape(id) for id in ids])
        
        # Filter the dataframe
        filtered_df = df[df['name'].str.contains(pattern, regex=True)]
        
        # Create sanitized output filename
        output_file = sanitize_filename(f"{tf}.filtered.bed")
        
        # Write the filtered data to a new BED file
        filtered_df.to_csv(output_file, sep='\t', header=False, index=False)

        print(f"Filtered BED file created for {tf}: {output_file}")

# Usage
tf_dict = {
    'MLX': ['MA0663.1']

    # Add more TF name: [TF_ID list] pairs as needed
}

filter_bed_by_multiple_tfs('IFN-Induced-Unaffected-24hr_CurrentSites.bed', tf_dict)
