#!/bin/bash
check_test() {
    local case="$1"
    local expected="$2"

    # also redirect stderr so that error messages from invalid 
    # programs are not shown 
    echo "$case" | ../src/./test > /dev/null 2>&1
    exit=$? 
    
    if [ $exit -ne 0 ] && [ $expected = "invalid" ]; then 
        echo "Correct Output for Test Case" 
    elif [ $exit -eq 0 ] && [ $expected = "valid" ]; then 
        echo "Correct Output for Test Case" 
    elif [ $exit -ne 0]; then
        echo "Outputted Valid for Invalid Test Case"
    else 
        echo "Outputted Invalid for Valid Test Case"
    fi
}

testcases=("int x;" "int y" "int (a, b){}" "int add(int a, int b){return 2;}" 
           "int add(int a, int b){ unrestricted int c[4]; c = [1, 2, 3, 4]; return 2;}")
expected=("valid" "invalid" "invalid" "valid" "valid")

for num in "${!testcases[@]}"; do 
    check_test "${testcases[$num]}" "${expected[$num]}"
done

