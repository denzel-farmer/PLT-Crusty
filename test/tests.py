''' Automated tester for Crusty compiler '''

import os
import subprocess
import sys


# Output Colors 
PASS = '\033[92m'
FAIL = '\033[91m'
ENDC = '\033[0m'

# File path constants 
SAMPLE_DIR = "samples-automated"

SEMANT_CHECKER_PATH = "../checksemant.native"
IR_GEN_PATH = "../crusty.native"
RUNNER_PATH = "/usr/bin/lli"

CRUST_SUFFIX = ".crust"
EXPECTED_SEMANT_SUFFIX = ".expected.sem"
OUT_SEMANT_SUFFIX = ".out.sem"
OUT_IR_SUFFIX = ".out.llvm"
OUT_RUN_SUFFIX = ".out"
EXPECTED_OUT_SUFFIX = ".expected.out"

TEST_LOG_SEMANT_SUFFIX = ".log.sem"
TEST_LOG_SUFFIX = ".log"

num_passed = 0
num_tests = 0

def print_short_pad_path(short_file_path):
    # Print short file path, but pad with spaces so the next print statement aligns
    print(f"{short_file_path[-80:]:80}", end="")


def run_irgen_test(irgen_path, sample_dir, sample_name):
    global num_passed
    global num_tests

    sample_base = f"{sample_dir}/{sample_name}"
    sample_path = f"{sample_base}{CRUST_SUFFIX}"

    run_output_path = f"{sample_base}{OUT_RUN_SUFFIX}"
    expected_output_path = f"{sample_base}{EXPECTED_OUT_SUFFIX}"

    num_tests = num_tests + 1

    print("[IRGEN] ", end="")
    print_short_pad_path(sample_name)

    # Generate IR
    ir_output_path = f"{sample_base}{OUT_IR_SUFFIX}"
    log_file_path = f"{sample_base}{TEST_LOG_SUFFIX}"

    with open(sample_path, "r") as input_file, open(ir_output_path, "w") as output_file, open(log_file_path, "w") as err_file:
            subprocess.run([irgen_path], stdin=input_file,
                            stdout=output_file, stderr=err_file)

    # Run the generated IR

    with open(ir_output_path, "r") as ir_file, open(run_output_path, "w") as output_file, open(log_file_path, "a") as err_file:
        subprocess.run([RUNNER_PATH], stdin=ir_file, stdout=output_file, stderr=err_file)

    # Compare the output with the expected output
    if os.path.isfile(expected_output_path):
        with open(expected_output_path, "r") as expected_file, open(run_output_path, "r") as actual_file:
            expected_output = expected_file.read()
            actual_output = actual_file.read()

            if expected_output != actual_output:
                print(f"{FAIL}FAILED{ENDC}")
            else:
                num_passed = num_passed + 1
                print(f"{PASS}PASSED{ENDC}")
    else:
        print(f"Expected output file not found for {sample_name}")

def run_semantic_test(semant_checker_path, sample_dir, sample_name):
    global num_passed
    global num_tests

    sample_base = f"{sample_dir}/{sample_name}"
    sample_path = f"{sample_base}{CRUST_SUFFIX}"

    print("[SEMANT] ", end="")
    print_short_pad_path(sample_name)

    num_tests = num_tests + 1


    # Paths
    expected_output_path = f"{sample_base}{EXPECTED_SEMANT_SUFFIX}"
    output_file_path = f"{sample_base}{OUT_SEMANT_SUFFIX}"
    log_file_path = f"{sample_base}{TEST_LOG_SEMANT_SUFFIX}"

    with open(sample_path, "r") as input_file, open(output_file_path, "w") as output_file, open(log_file_path, "w") as err_file:
            subprocess.run([semant_checker_path], stdin=input_file,
                            stdout=output_file, stderr=err_file)

    # Compare the output with the expected output
    if os.path.isfile(expected_output_path):
        with open(expected_output_path, "r") as expected_file, open(output_file_path, "r") as actual_file:
            expected_output = expected_file.read()
            actual_output = actual_file.read()

            if expected_output != actual_output:
                print(f"{FAIL}FAILED{ENDC}")
            else:
                print(f"{PASS}PASSED{ENDC}")
                num_passed = num_passed + 1
    else:
        print(f"Expected output file not found for {sample_name}")


def run_test_irgen_suite(suite_dir):
    # Get base name from suite_dir as module name
    module_name = os.path.basename(suite_dir)
    print(f"Running tests for module: {module_name}")

    # Get all folders in the suite dir
    sample_dirs = [f.path for f in os.scandir(suite_dir) if f.is_dir()]
    for sample_dir in sample_dirs:
        sample_name = os.path.basename(sample_dir)
        run_irgen_test(IR_GEN_PATH, sample_dir, sample_name)

def run_test_semantic_suite(suite_dir):
    # Get base name from suite_dir as module name
    module_name = os.path.basename(suite_dir)
    print(f"Running tests for module: {module_name}")

    # Get all folders in the suite dir
    sample_dirs = [f.path for f in os.scandir(suite_dir) if f.is_dir()]
    for sample_dir in sample_dirs:
        sample_name = os.path.basename(sample_dir)
        run_semantic_test(SEMANT_CHECKER_PATH, sample_dir, sample_name)

def run_tests():
    global num_passed
    global num_tests

    # Check that SEMANT_CHECKER_PATH exists
    if not os.path.isfile(SEMANT_CHECKER_PATH):
        print(f"Error: {SEMANT_CHECKER_PATH} not found")
        return

    # Extract all subdirs from SAMPLE_DIR as a test suite

    test_suites = [f.path for f in os.scandir(SAMPLE_DIR) if f.is_dir()]

    print("Starting semantic tests")

    for suite_dir in test_suites:
        run_test_semantic_suite(suite_dir)

    # Display number passed out of total
    print(f"Passed {num_passed} out of {num_tests} semantic tests")

    print("Starting IRGen tests")
    total_tests = num_tests 
    total_passed = num_passed
    num_tests = 0
    num_passed = 0
    
    for suite_dir in test_suites:
        run_test_irgen_suite(suite_dir)

    # Display number passed out of total
    print(f"Passed {num_passed} out of {num_tests} IRGen tests")

    total_tests += num_tests
    total_passed += num_passed

    print(f"Total: Passed {total_passed} out of {total_tests} tests")

run_tests()