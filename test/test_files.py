import os
import subprocess
import sys

# Check if the binary path is provided as an argument
if len(sys.argv) < 2:
    print("Usage: python test_files.py <binary_path> [-f <string>]")
    exit(1)

binary_path = sys.argv[1]
samples_folder = "samples"
test_file = "test-file.expected"
test_output_folder = "test-output"

# Check if the -f option is provided
if len(sys.argv) > 2 and sys.argv[2] == "-f":
    # Check if the filename is provided
    if len(sys.argv) < 4:
        print("Usage: python test_files.py <binary_path> [-f <string>]")
        exit(1)

    # Run the binary on the specified file and print the output
    filename = sys.argv[3]
    print(f"Running binary on file: {samples_folder}/{filename}")
    with open(f"{samples_folder}/{filename}", "r") as file:
        subprocess.run([binary_path], stdin=file, stdout=sys.stdout, stderr=sys.stdout)
else:
    # Iterate over all files in the 'samples' folder
    for root, dirs, files in os.walk(os.path.join(os.getcwd(), samples_folder)):
        for file in files:
            if file.endswith(".crust"):
                # Extract the filename without extension
                filename = os.path.splitext(file)[0]

                file_path = os.path.join(root, file)
                os.makedirs(test_output_folder, exist_ok=True)
                output_file = f"{test_output_folder}/{filename}.crust.out"
                print(f"Running binary on file: {filename}")
                # Run the binary on the current file and save the output
                with open(file_path, "r") as input_file, open(output_file, "w") as output_file:
                    subprocess.run([binary_path], stdin=input_file, stdout=output_file, stderr=output_file)

                # Compare the output with the expected output
                expected_output_file = os.path.join(samples_folder, f"{filename}.expected")
                if os.path.isfile(expected_output_file):
                    print("Comparing output with expected...")
                    with open(expected_output_file, "r") as expected_file, open(output_file, "r") as actual_file:
                        expected_output = expected_file.read()
                        actual_output = actual_file.read()

                        print(f"Comparing output for file: {file_path}")
                        print(f"Expected output: {expected_output}")
                        print(f"Actual output: {actual_output}")

                        if expected_output != actual_output:
                            print(f"FAILED: {filename}")
                else:
                    print(f"Expected output file not found for {filename}")