./sudoku `sudoku -g10 -cfiendish -f compact | grep "[.]" | tr -d "\n" | sed 's/.\{81\}/&\n/g'` +RTS -p
