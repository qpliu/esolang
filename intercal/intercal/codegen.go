package intercal

import (
	"fmt"
	"io"
)

func CodeGen(statements []*Statement, w io.Writer) error {
	// program listing constant for listing, error 000 message,
	// and statement numbers for error messages
	listingIndexes := [][2]int{}
	statementNumberIndexes := [][2]int{}
	listingSize := 0
	for i, stmt := range statements {
		stmtNumber := fmt.Sprintf("%04d", i+1)
		stmtNumberIndex := listingSize + 1
		listingSize += 2 + len(stmtNumber)
		startIndex := listingSize
		listingSize += len(stmt.String()) + 1
		listingIndexes = append(listingIndexes, [2]int{startIndex, len(stmt.String())})
		statementNumberIndexes = append(statementNumberIndexes, [2]int{stmtNumberIndex, len(stmtNumber)})
	}
	statementNumberIndexes = append(statementNumberIndexes, [2]int{listingSize, 4}) // "nnnn" for error 633 or abstained

	if _, err := fmt.Fprintf(w, "@program_listing = private constant [%d x i8] [", listingSize+4); err != nil {
		return err
	}
	for i, stmt := range statements {
		mark := " "
		if stmt.Type == StatementUnrecognizable {
			mark = "*"
		}
		for _, b := range []byte(fmt.Sprintf("%s%04d %s\n", mark, i+1, stmt.String())) {
			if _, err := fmt.Fprintf(w, "i8 %d,", b); err != nil {
				return err
			}
		}
	}
	// "nnnn"
	if _, err := fmt.Fprintf(w, "i8 110,i8 110,i8 110,i8 110]\n"); err != nil {
		return err
	}

	// statement number string/length constant array
	if _, err := fmt.Fprintf(w, "@statement_numbers = private constant [%d x {i8*,i32}] [", len(statementNumberIndexes)); err != nil {
		return err
	}
	for i, index := range statementNumberIndexes {
		comma := ","
		if i == 0 {
			comma = ""
		}
		if _, err := fmt.Fprintf(w, "%s{i8*,i32}{i8* getelementptr([%d x i8], [%d x i8]* @program_listing, i32 0, i32 %d),i32 %d}", comma, listingSize+4, listingSize+4, index[0], index[1]); err != nil {
			return err
		}
	}
	if _, err := fmt.Fprintf(w, "]\n"); err != nil {
		return err
	}

	// abstain flag global array
	if _, err := fmt.Fprintf(w, "@abstain_flags = global [%d x i1] [", len(statements)+1); err != nil {
		return err
	}
	for _, stmt := range statements {
		flag := 1
		if stmt.Not {
			flag = 0
		}
		if _, err := fmt.Fprintf(w, "i1 %d,", flag); err != nil {
			return err
		}
	}
	// for statement nnnn after last statement (should never be abstained)
	// used when printing error messages
	if _, err := fmt.Fprintf(w, "i1 0]\n"); err != nil {
		return err
	}

	// error messages
	errorMessagesSize := 0
	errorMessageIndexes := [][3]int{} // error code, index, length
	for _, e := range ErrorList {
		msg := e.Message()
		if msg == "" {
			errorMessageIndexes = append(errorMessageIndexes, [3]int{e.Code(), 0, 0})
		} else {
			m := fmt.Sprintf("ICL%03dI %s", e.Code(), msg)
			errorMessageIndexes = append(errorMessageIndexes, [3]int{e.Code(), errorMessagesSize, len([]byte(m))})
			errorMessagesSize += len([]byte(m))
		}
	}
	if _, err := fmt.Fprintf(w, "@error_messages = private constant [%d x i8] [", errorMessagesSize); err != nil {
		return err
	}
	{
		comma := ""
		for _, e := range ErrorList {
			msg := e.Message()
			if msg != "" {
				m := fmt.Sprintf("ICL%03dI %s", e.Code(), msg)
				for _, b := range []byte(m) {
					if _, err := fmt.Fprintf(w, "%si8 %d", comma, b); err != nil {
						return err
					}
					comma = ","
				}
			}
		}
	}
	if _, err := fmt.Fprintf(w, "]\n"); err != nil {
		return err
	}

	// void @check_error(%val %arg, i32 %next_stmt_index)
	if _, err := fmt.Fprintf(w, "define void @check_error(%%val %%arg, i32 %%next_stmt_index) {\n    %%tag = extractvalue %%val %%arg, 0\n    %%iserror = icmp uge i2 %%tag, 2\n    br i1 %%iserror, label %%fatal_error, label %%ok\n  fatal_error:\n    call void @fatal_error(%%val %%arg, i32 %%next_stmt_index)\n    ret void\n  ok:\n    ret void\n}\n"); err != nil {
		return err
	}

	// void @fatal_error(%val %err, i32 %next_stmt_index) noreturn
	if _, err := fmt.Fprintf(w, "define void @fatal_error(%%val %%err, i32 %%next_stmt_index) noreturn {\n  entry:\n    br label %%find_stmt_index\n  find_stmt_index:\n    %%stmt_index = phi i32 [%%next_stmt_index,%%entry],[%%stmt_index1,%%find_stmt_index]\n    %%stmt_index1 = add i32 %%stmt_index, 1\n    %%abstained_ptr = getelementptr [%d x i1], [%d x i1]* @abstain_flags, i32 0, i32 %%stmt_index\n    %%abstained = load i1, i1* %%abstained_ptr\n    br i1 %%abstained, label %%find_stmt_index, label %%check_err_tag\n", len(statements)+1, len(statements)+1); err != nil {
		return err
	}
	if _, err := fmt.Fprintf(w, "  check_err_tag:\n    %%stmt_number_info_ptr = getelementptr [%d x {i8*,i32}], [%d x {i8*,i32}]* @statement_numbers, i32 0, i32 %%stmt_index\n    %%stmt_number_info = load {i8*,i32}, {i8*,i32}* %%stmt_number_info_ptr\n    %%stmt_number = extractvalue {i8*,i32} %%stmt_number_info, 0\n    %%stmt_number_len = extractvalue {i8*,i32} %%stmt_number_info, 1\n    %%tag = extractvalue %%val %%err, 0\n    %%code = extractvalue %%val %%err, 1\n    %%tag_is2 = icmp eq i2 %%tag, 2\n    br i1 %%tag_is2, label %%err_code_index0, label %%err_nomsg\n  err_nomsg:\n    call void @finish_fatal_error(i8* null,i32 0,i8* %%stmt_number,i32 %%stmt_number_len) noreturn\n    ret void\n", len(statements)+1, len(statements)+1); err != nil {
		return err
	}
	{
		for i, indexes := range errorMessageIndexes {
			if _, err := fmt.Fprintf(w, "  err_code_index%d:\n    %%check_err_code%d = icmp eq i32 %%code, %d\n    br i1 %%check_err_code%d, label %%err_code%d, label %%err_code_index%d\n  err_code%d:\n    call void @finish_fatal_error(i8* getelementptr([%d x i8], [%d x i8]* @error_messages, i32 0, i32 %d), i32 %d, i8* %%stmt_number, i32 %%stmt_number_len) noreturn\n    ret void\n", i, i, indexes[0], i, indexes[0], i+1, indexes[0], errorMessagesSize, errorMessagesSize, indexes[1], indexes[2]); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(w, "  err_code_index%d:\n    br label %%err_code778\n}\n", len(errorMessageIndexes)); err != nil {
			return err
		}
	}

	//...
	//... globals for variables and arrays

	// void @main()
	if _, err := fmt.Fprintf(w, "define void @main() {\n    call i32 @write(i32 1, i8* getelementptr([%d x i8],[%d x i8]* @program_listing,i32 0,i32 0),i32 %d)\n    %%random_seed = call i32 @time(i8* null)\n    call void @srandom(i32 %%random_seed)\n    br label %%stmt0\n", listingSize+4, listingSize+4, listingSize); err != nil {
		return err
	}

	//...
	//... code for each statement
	if _, err := fmt.Fprintf(w, "  stmt0:\n    br label %%stmt%d\n", len(statements)); err != nil {
		return err
	}

	if _, err := fmt.Fprintf(w, "  stmt%d:\n    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 633,1),i32 %d)\n    ret void\n}\n", len(statements), len(statements)); err != nil {
		return err
	}

	return nil
}
