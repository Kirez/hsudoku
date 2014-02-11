./sudoku `sudoku -g10 -ceasy -f compact | grep "[.]" | tr -d "\n" | sed 's/.\{81\}/&\n/g'` +RTS -p
