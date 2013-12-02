sudoku -g10 -ceasy -f compact | grep "[.]" | tr -d "\n" | sed 's/.\{81\}/&\n/g' | tee gensu.txt && time ./Sudoku gensu.txt result.txt && cat result.txt
