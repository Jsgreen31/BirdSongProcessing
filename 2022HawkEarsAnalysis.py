#### Testing code

import os
import subprocess
from multiprocessing import Pool
from pathlib import Path
from tqdm import tqdm  # Import tqdm for the progress bar

# Function to analyze a single recording or directory using the HawkEars analyze.py script
def analyze_recording(recording_path, output_path):
    # Ensure the output directory exists
    if not os.path.exists(output_path):
        print(f"Creating output directory: {output_path}")
        os.makedirs(output_path)

    # Run the HawkEars analysis script for each recording
    command = ['python', 'C:/Users/jogreen/Desktop/HawkEars/analyze.py', '-i', str(recording_path), '-o', str(output_path)]

    # Print the command being run (for debugging)
    print(f"Running command: {' '.join(command)}")

    try:
        # Run the command and capture the output and error messages
        result = subprocess.run(command, check=True, capture_output=True, text=True)
        print(f"Success: {recording_path} analyzed successfully.")
        print(f"Output: {result.stdout}")
    except subprocess.CalledProcessError as e:
        print(f"Error analyzing {recording_path}: {e}")
        print(f"Error Output: {e.stderr}")

# Wrapper function to add the progress bar
def analyze_recording_with_progress(recording_and_paths):
    recording_path, output_path = recording_and_paths
    analyze_recording(recording_path, output_path)

# Function to process all recordings in parallel
def process_directory_in_parallel(input_directory, output_directory):
    # Ensure the output directory exists
    if not os.path.exists(output_directory):
        print(f"Creating output directory: {output_directory}")
        os.makedirs(output_directory, exist_ok=True)

    # Get a list of all recording files (e.g., WAV files) in the directory and subdirectories
    recordings = [f for f in Path(input_directory).rglob('*.wav')]  # Recursive search for .wav files
    total_recordings = len(recordings)

    # Verify that recordings were found
    if total_recordings == 0:
        print(f"No .wav recordings found in {input_directory}.")
        return

    print(f"Found {total_recordings} recordings to analyze.")

    # Create a pool of processes to analyze recordings in parallel
    with Pool() as pool:
        # Use tqdm to track the progress of each task in the pool
        for _ in tqdm(pool.imap(analyze_recording_with_progress,
                                [(recording, output_directory) for recording in recordings]),
                      total=total_recordings,
                      desc="Analyzing recordings",
                      ncols=100):
            pass  # Just iterate to show the progress bar

if __name__ == '__main__':
    # Define the paths (replace with actual paths)
    input_directory = 'E:/ARU Data/2022/Gagetown'  # Directory containing bird song recordings
    output_directory = 'C:/Users/jogreen/Desktop/HawkEars Outputs/Gagetown 2022'  # Directory to save results

    # Process the recordings in parallel
    process_directory_in_parallel(input_directory, output_directory)
