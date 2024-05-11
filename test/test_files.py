import os
import subprocess
import sys

PASS = '\033[92m'
FAIL = '\033[91m'
ENDC = '\033[0m'


def linear_check_success(output):
    return "LINEAR CHECK SUCCESS" in output


def compile_success(output):
    return "COMPILATION SUCCESS" in output


def check_output(output, expected_valid):
    if expected_valid:
        if not compile_success(output):
            print(
                f"{FAIL}FAILED - compilation failed, but should have succeeded {ENDC}")
        elif not linear_check_success(output):
            print(
                f"{FAIL}FAILED - linear check failed, but should have succeeded {ENDC}")
        else:
            print(f"{PASS}PASSED{ENDC}")
    else:
        if compile_success(output) and linear_check_success(output):
            print(
                f"{FAIL}FAILED - compilation and linear check succeeded, but at least one should have failed {ENDC}")
        else:
            print(f"{PASS}PASSED{ENDC}")


# Check if the binary path is provided as an argument
if len(sys.argv) < 2:
    print("Usage: python test_files.py <binary_path> [-f <string>] [-c]")
    exit(1)

binary_path = sys.argv[1]
samples_folder = "samples"
valid_folder = "valid"
invalid_folder = "invalid"
test_file = "test-file.expected"
test_output_folder = "test-output"
compare = False

# Check if the -c option is provided
if "-c" in sys.argv:
    compare = True

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
        subprocess.run([binary_path], stdin=file,
                       stdout=sys.stdout, stderr=sys.stdout)
else:
    # Iterate over all files in the 'samples' folder
    for root, dirs, files in os.walk(os.path.join(os.getcwd(), samples_folder)):
        for file in files:
            if file.endswith(".crust"):
                # Extract the filename without extension
                filename = os.path.splitext(file)[0]

                file_path = os.path.join(root, file)
                os.makedirs(test_output_folder, exist_ok=True)
                output_file_path = f"{test_output_folder}/{filename}.crust.out"

                parent_dir = os.path.basename(os.path.dirname(file_path))
                
                # Get grandparent directory 
                grandparent_dir = os.path.basename(os.path.dirname(os.path.dirname(file_path)))

                short_file_path = os.path.relpath(file_path, samples_folder)
                
                # Print short file path, but pad with spaces so the next print statement aligns
                print(f"{short_file_path[-80:]:80}", end="")

                # print(f"{grandparent_dir}/{parent_dir}/{filename}: ", end="")
                # Run the binary on the current file and save the output
                with open(file_path, "r") as input_file, open(output_file_path, "w") as output_file:
                    subprocess.run([binary_path], stdin=input_file,
                                   stdout=output_file, stderr=output_file)

                if compare:
                    # Compare the output with the expected output
                    expected_output_file = os.path.join(
                        samples_folder, f"{filename}.expected")
                    if os.path.isfile(expected_output_file):
                        print("Comparing output with expected...")
                        with open(expected_output_file, "r") as expected_file, open(output_file_path, "r") as actual_file:
                            expected_output = expected_file.read()
                            actual_output = actual_file.read()

                            print(f"Comparing output for file: {file_path}")
                            print(f"Expected output: {expected_output}")
                            print(f"Actual output: {actual_output}")

                            if expected_output != actual_output:
                                print(f"FAILED: {filename}")
                    else:
                        print(f"Expected output file not found for {filename}")
                else:
                    # Print if scanning and parsing passed
                    with open(output_file_path, "r") as actual_file:
                        actual_output = actual_file.read()

                        # If any parent directory is 'valid', fail if compilation or linear checking failed
                        if (valid_folder == parent_dir):
                            check_output(actual_output, True)
                        elif (invalid_folder == parent_dir):
                            check_output(actual_output, False)
                        else:
                            print(
                                "No expected result, printing compilation and linear check results")
                            if (compile_success(actual_output)):
                                print("Compilation success")
                            else:
                                print("Compilation failed")
                            if (linear_check_success(actual_output)):
                                print("Linear check success")
                            else:
                                print("Linear check failed")
