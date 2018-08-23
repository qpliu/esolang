package intercal

import (
	"fmt"
	"io"
)

func CodeGen(statements []*Statement, w io.Writer) error {
	listingIndexes := [][2]int{}
	listingSize := 0
	for i, stmt := range statements {
		stmtNumber := fmt.Sprintf("%04d", i+1)
		listingSize += 2 + len(stmtNumber)
		startIndex := listingSize
		listingSize += len(stmt.String()) + 1
		listingIndexes = append(listingIndexes, [2]int{startIndex, len(stmt.String())})
	}

	if _, err := fmt.Fprintf(w, "@program_listing = private constant [%d x i8] [", listingSize); err != nil {
		return err
	}
	comma := ""
	for i, stmt := range statements {
		mark := " "
		if stmt.Type == StatementUnrecognizable {
			mark = "*"
		}
		for _, b := range []byte(fmt.Sprintf("%s%04d %s\n", mark, i+1, stmt.String())) {
			if _, err := fmt.Fprintf(w, "%si8 %d", comma, b); err != nil {
				return err
			}
			comma = ","
		}
	}
	if _, err := fmt.Fprintf(w, "]\n"); err != nil {
		return err
	}

	//...
	//... global for abstain array
	//... globals for variables and arrays
	//... constants for error messages
	//... constants for line numbers + "nnnn"

	if _, err := fmt.Fprintf(w, "define void @main() {\n    call i32 @write(i32 1, i8* getelementptr([%d x i8],[%d x i8]* @program_listing,i32 0,i32 0),i32 %d)\n    %%random_seed = call i32 @time(i8* null)\n    call void @srandom(i32 %%random_seed)\n", listingSize, listingSize, listingSize); err != nil {
		return err
	}

	//...
	//... code for each statement

	if _, err := fmt.Fprintf(w, "    call void @exit(i32 0)\n    ret void\n}\n"); err != nil {
		return err
	}

	return nil
}
