import os
import shutil

TARGET_FOLDER = "../../../crustsamples/linear"

# Get the current directory
current_directory = os.getcwd()

# Get a list of all files in the current directory
files = os.listdir(current_directory)

# Iterate over each file
for file in files:
    # Check if the file ends with ".crust"
    if file.endswith(".crust"):
        # Remove the ".crust" extension from the file name
        new_folder_name = file[:-6]

        # Create the target folder if it doesn't exist
        target_folder = os.path.join(TARGET_FOLDER, new_folder_name)
        os.makedirs(target_folder, exist_ok=True)

        # Move the file to the target folder
        source_file = os.path.join(current_directory, file)
        shutil.move(source_file, target_folder)