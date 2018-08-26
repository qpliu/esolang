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
		flag := 0
		if stmt.Not {
			flag = 1
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
	if _, err := fmt.Fprintf(w, "define void @check_error(%%val %%arg, i32 %%next_stmt_index) {\n    %%tag = extractvalue %%val %%arg, 0\n    %%iserror = icmp uge i2 %%tag, 2\n    br i1 %%iserror, label %%fatal_error, label %%ok\n  fatal_error:\n    call void @fatal_error(%%val %%arg, i32 %%next_stmt_index) noreturn\n    ret void\n  ok:\n    ret void\n}\n"); err != nil {
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

	// globals for variables and arrays
	if err := codeGenVariables(w, statements); err != nil {
		return err
	}

	// void @main()
	if _, err := fmt.Fprintf(w, "define void @main() {\n    call i32 @write(i32 1, i8* getelementptr([%d x i8],[%d x i8]* @program_listing,i32 0,i32 0),i32 %d)\n    %%random_seed = call i32 @time(i8* null)\n    call void @srandom(i32 %%random_seed)\n    br label %%stmt0\n", listingSize+4, listingSize+4, listingSize); err != nil {
		return err
	}

	for i, stmt := range statements {
		if err := codeGenStmt(w, stmt, statements, listingIndexes[i], listingSize); err != nil {
			return err
		}
	}

	if _, err := fmt.Fprintf(w, "  stmt%d:\n    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 633,1),i32 %d) noreturn\n    ret void\n}\n", len(statements), len(statements)); err != nil {
		return err
	}

	return nil
}

func nextIndex(stmt *Statement, statements []*Statement) int {
	switch stmt.Type {
	case StatementNext:
		return stmt.Operands.(int)
	case StatementResume:
		return len(statements)
	default:
		return stmt.Index + 1
	}
}

func codeGenStmt(w io.Writer, stmt *Statement, statements []*Statement, listingIndexes [2]int, listingSize int) error {
	if _, err := fmt.Fprintf(w, "  stmt%d:\n", stmt.Index); err != nil {
		return err
	}
	labelCounter := 0

	// Err774 check
	{
		doErr774 := stmt.Please && stmt.Type != StatementUnrecognizable
		for i := stmt.Index + 1; i < len(statements) && i <= stmt.Index+10; i++ {
			doErr774 = doErr774 && !statements[i].Thank
		}
		if doErr774 {
			ident1 := fmt.Sprintf("%%stmt%d.%d", stmt.Index, labelCounter)
			label1 := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+1)
			label2 := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+2)
			labelCounter += 3
			if _, err := fmt.Fprintf(w, "    %s = call i1 @random_check(i32 9)\n    br i1 %s, label %%%s, label %%%s\n  %s:\n    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 774,1),i32 %d) noreturn\n    ret void\n  %s:\n", ident1, ident1, label1, label2, label1, nextIndex(stmt, statements), label2); err != nil {
				return err
			}
		}
	}

	// ABSTAIN check
	{
		ident1 := fmt.Sprintf("%%abstainptr%d.%d", stmt.Index, labelCounter)
		ident2 := fmt.Sprintf("%%abstain%d.%d", stmt.Index, labelCounter+1)
		label1 := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+2)
		labelCounter += 3
		if _, err := fmt.Fprintf(w, "    %s = getelementptr [%d x i1], [%d x i1]* @abstain_flags,i32 0,i32 %d\n    %s = load i1, i1* %s\n    br i1 %s, label %%stmt%d, label %%%s\n  %s:\n", ident1, len(statements)+1, len(statements)+1, stmt.Index, ident2, ident1, ident2, stmt.Index+1, label1, label1); err != nil {
			return err
		}
	}

	if stmt.Chance == 0 {
		if _, err := fmt.Fprintf(w, "    br label %%stmt%d\n", stmt.Index+1); err != nil {
			return err
		}
	}

	if stmt.Error != nil && stmt.Error.Message() != "" {
		if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 %d,1),i32 %d) noreturn\n    br label %%stmt%d\n", stmt.Error.Code(), stmt.Index+1, stmt.Index+1); err != nil {
			return err
		}
		return nil
	}

	if stmt.Type == StatementUnrecognizable {
		if _, err := fmt.Fprintf(w, "    ;SYNTAX ERROR\n    call i32 @write(i32 2, i8* getelementptr([8 x i8], [8 x i8]* @error_message_000_prefix,i32 0,i32 0),i32 8)\n    call i32 @write(i32 2, i8* getelementptr([%d x i8], [%d x i8]* @program_listing,i32 0,i32 %d),i32 %d)\n", listingSize+4, listingSize+4, listingIndexes[0], listingIndexes[1]); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 3,0),i32 0,1),i32 %d) noreturn\n    br label %%stmt%d\n", stmt.Index+1, stmt.Index+1); err != nil {
			return err
		}
		return nil
	}

	// probability check
	redoLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter)
	redoDoneLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+1)
	labelCounter += 2
	{
		doLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter)
		startStmtLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+1)
		countCheckLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+2)
		countIdent := fmt.Sprintf("%%docount%d.%d", stmt.Index, labelCounter+3)
		nextCountIdent := fmt.Sprintf("%%nextdocount%d.%d", stmt.Index, labelCounter+4)
		labelCounter += 5
		if _, err := fmt.Fprintf(w, "    br label %%%s\n  %s:\n    br label %%%s\n  %s:\n    br label %%%s\n  %s:\n", doLabel, doLabel, countCheckLabel, redoLabel, countCheckLabel, countCheckLabel); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    %s = phi i32 [0,%%%s], [%s,%%%s]\n", countIdent, doLabel, nextCountIdent, redoLabel); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    %s = add i32 %s, 1\n", nextCountIdent, countIdent); err != nil {
			return err
		}

		countLimitIdent := fmt.Sprintf("%%countcheck%d.%d", stmt.Index, labelCounter)
		randomCheckLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+1)
		labelCounter += 2
		if _, err := fmt.Fprintf(w, "    %s = icmp ult i32 %s, %d\n    br i1 %s, label %%%s, label %%%s\n  %s:\n", countLimitIdent, countIdent, stmt.Chance/100, countLimitIdent, startStmtLabel, randomCheckLabel, randomCheckLabel); err != nil {
			return err
		}

		if stmt.Chance%100 == 0 {
			if _, err := fmt.Fprintf(w, "    br label %%%s\n", redoDoneLabel); err != nil {
				return err
			}
		} else {
			issecondRandomCheckIdent := fmt.Sprintf("%%issecondrandomcheck%d.%d", stmt.Index, labelCounter)
			firstRandomCheckLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter)
			labelCounter += 2
			if _, err := fmt.Fprintf(w, "    %s = icmp ugt i32 %s, %d\n    br i1 %s, label %%%s, label %%%s\n  %s:\n", issecondRandomCheckIdent, countIdent, stmt.Chance/100, issecondRandomCheckIdent, redoDoneLabel, firstRandomCheckLabel, firstRandomCheckLabel); err != nil {
				return err
			}
			randomCheckResult := fmt.Sprintf("%%randomcheck%d.%d", stmt.Index, labelCounter)
			labelCounter++
			doneLabel := redoDoneLabel
			if stmt.Chance <= 100 {
				doneLabel = fmt.Sprintf("stmt%d", stmt.Index+1)
			}
			if _, err := fmt.Fprintf(w, "    %s = call i1 @random_check(i32 %d)\n    br i1 %s, label %%%s, label %%%s\n", randomCheckResult, 10*(stmt.Chance%100), randomCheckResult, startStmtLabel, doneLabel); err != nil {
			}
		}
		if _, err := fmt.Fprintf(w, "  %s:\n", startStmtLabel); err != nil {
			return err
		}
	}

	if stmt.Error != nil {
		if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 %d,1),i32 %d)\n    br label %%stmt%d\n", stmt.Error.Code(), stmt.Index+1, stmt.Index+1); err != nil {
			return err
		}
		return nil
	}

	switch stmt.Type {
	case StatementNext:
		stackptrIdent := fmt.Sprintf("%%stackptr%d.%d", stmt.Index, labelCounter)
		stackptrCheckIdent := fmt.Sprintf("%%stackptrcheck%d.%d", stmt.Index, labelCounter+1)
		stackptrOverflowLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+2)
		stackptrOkLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+3)
		labelCounter += 4
		if _, err := fmt.Fprintf(w, "    ;NEXT\n    %s = load i32, i32* @stackptr\n    %s = icmp uge i32 %s, 79\n    br i1 %s, label %%%s, label %%%s\n", stackptrIdent, stackptrCheckIdent, stackptrIdent, stackptrCheckIdent, stackptrOverflowLabel, stackptrOkLabel); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "  %s:\n    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 123,1),i32 %d)\n    br label %%%s\n", stackptrOverflowLabel, nextIndex(stmt, statements), redoLabel); err != nil {
			return err
		}
		nextStackptrIdent := fmt.Sprintf("%%nextstackptr%d.%d", stmt.Index, labelCounter)
		stackentryptrIdent := fmt.Sprintf("%%stackentryptr%d.%d", stmt.Index, labelCounter+1)
		labelCounter += 2
		if _, err := fmt.Fprintf(w, "  %s:\n    %s = add i32 %s, 1\n    store i32 %s, i32* @stackptr\n    %s = getelementptr [79 x i8*], [79 x i8*]* @stack, i32 0, i32 %s\n    store i8* blockaddress(@main,%%stmt%d), i8** %s\n    br label %%%s\n", stackptrOkLabel, nextStackptrIdent, stackptrIdent, nextStackptrIdent, stackentryptrIdent, stackptrIdent, stmt.Index+1, stackentryptrIdent, redoLabel); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "  %s:\n    br label %%stmt%d\n", redoDoneLabel, nextIndex(stmt, statements)); err != nil {
			return err
		}

	case StatementForget:
		if _, err := fmt.Fprintf(w, "    ;FORGET\n"); err != nil {
			return err
		}
		argValIdent := ""
		if newLabelCounter, exprIdent, err := codeGenExpr(w, stmt, stmt.Operands.(Expr), labelCounter); err != nil {
			return err
		} else {
			labelCounter = newLabelCounter
			argValIdent = exprIdent
		}
		if _, err := fmt.Fprintf(w, "    call void @check_error(%%val %s, i32 %d)\n", argValIdent, nextIndex(stmt, statements)); err != nil {
			return err
		}
		argIdent := fmt.Sprintf("%%arg%d.%d", stmt.Index, labelCounter)
		labelCounter++
		if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s, 1\n", argIdent, argValIdent); err != nil {
			return err
		}
		stackptrIdent := fmt.Sprintf("%%stackptr%d.%d", stmt.Index, labelCounter)
		labelCounter++
		if _, err := fmt.Fprintf(w, "    %s = load i32, i32* @stackptr\n", stackptrIdent); err != nil {
			return err
		}

		stackptrCheckIdent := fmt.Sprintf("%%stackptrcheck%d.%d", stmt.Index, labelCounter)
		labelCounter++
		if _, err := fmt.Fprintf(w, "    %s = icmp uge i32 %s, %s\n", stackptrCheckIdent, argIdent, stackptrIdent); err != nil {
			return err
		}

		stackptrUnderflowLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter)
		stackptrOkLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+1)
		nextStackptrIdent := fmt.Sprintf("%%nextstackptr%d.%d", stmt.Index, labelCounter+2)
		labelCounter += 3
		if _, err := fmt.Fprintf(w, "    br i1 %s, label %%%s, label %%%s\n", stackptrCheckIdent, stackptrUnderflowLabel, stackptrOkLabel); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "  %s:\n    store i32 0,i32* @stackptr\n    br label %%%s\n", stackptrUnderflowLabel, redoLabel); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "  %s:\n    %s = sub i32 %s, %s\n    store i32 %s,i32* @stackptr\n    br label %%%s\n", stackptrOkLabel, nextStackptrIdent, stackptrIdent, argIdent, nextStackptrIdent, redoLabel); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "  %s:\n    br label %%stmt%d\n", redoDoneLabel, nextIndex(stmt, statements)); err != nil {
			return err
		}

	case StatementResume:
		if _, err := fmt.Fprintf(w, "    ;RESUME\n"); err != nil {
			return err
		}
		argValIdent := ""
		if newLabelCounter, exprIdent, err := codeGenExpr(w, stmt, stmt.Operands.(Expr), labelCounter); err != nil {
			return err
		} else {
			labelCounter = newLabelCounter
			argValIdent = exprIdent
		}
		if _, err := fmt.Fprintf(w, "    call void @check_error(%%val %s, i32 %d)\n", argValIdent, nextIndex(stmt, statements)); err != nil {
			return err
		}
		argIdent := fmt.Sprintf("%%arg%d.%d", stmt.Index, labelCounter)
		labelCounter++
		if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s, 1\n", argIdent, argValIdent); err != nil {
			return err
		}

		argiszeroIdent := fmt.Sprintf("%%argiszero%d.%d", stmt.Index, labelCounter)
		argiszeroLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+1)
		argisnotzeroLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+2)
		labelCounter += 3
		if _, err := fmt.Fprintf(w, "    %s = icmp eq i32 %s, 0\n", argiszeroIdent, argIdent); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    br i1 %s, label %%%s, label %%%s\n", argiszeroIdent, argiszeroLabel, argisnotzeroLabel); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "  %s:\n    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 621,1),i32 %d)\n    br label %%%s\n", argiszeroLabel, nextIndex(stmt, statements), redoLabel); err != nil {
			return err
		}

		if _, err := fmt.Fprintf(w, "  %s:\n", argisnotzeroLabel); err != nil {
			return err
		}
		stackptrIdent := fmt.Sprintf("%%stackptr%d.%d", stmt.Index, labelCounter)
		checkunderflowIdent := fmt.Sprintf("%%checkunderflow%d.%d", stmt.Index, labelCounter+1)
		underflowLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+2)
		nounderflowLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+3)
		labelCounter += 4
		if _, err := fmt.Fprintf(w, "    %s = load i32, i32* @stackptr\n", stackptrIdent); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    %s = icmp ugt i32 %s, %s\n", checkunderflowIdent, argIdent, stackptrIdent); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    br i1 %s, label %%%s, label %%%s\n", checkunderflowIdent, underflowLabel, nounderflowLabel); err != nil {
			return err
		}

		if _, err := fmt.Fprintf(w, "  %s:\n    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 632,1),i32 %d)\n    br label %%%s\n", underflowLabel, nextIndex(stmt, statements), redoLabel); err != nil {
			return err
		}

		newstackptrIdent := fmt.Sprintf("%%newstackptr%d.%d", stmt.Index, labelCounter)
		labelCounter++
		if _, err := fmt.Fprintf(w, "  %s:\n    %s = sub i32 %s, %s\n", nounderflowLabel, newstackptrIdent, stackptrIdent, argIdent); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    store i32 %s, i32* @stackptr\n", newstackptrIdent); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    br label %%%s", redoLabel); err != nil {
			return err
		}

		if _, err := fmt.Fprintf(w, "  %s:\n", redoDoneLabel); err != nil {
			return err
		}
		finalstackptrIdent := fmt.Sprintf("%%finalstackptr%d.%d", stmt.Index, labelCounter)
		finalstackentryptrIdent := fmt.Sprintf("%%finalstackentryptr%d.%d", stmt.Index, labelCounter+1)
		nextStmtIdent := fmt.Sprintf("%%nextstmt%d.%d", stmt.Index, labelCounter+2)
		labelCounter += 3
		if _, err := fmt.Fprintf(w, "    %s = load i32, i32* @stackptr\n", finalstackptrIdent); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    %s = getelementptr [79 x i8*], [79 x i8*]* @stack,i32 0,i32 %s\n", finalstackentryptrIdent, finalstackptrIdent); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    %s = load i8*, i8** %s\n", nextStmtIdent, finalstackentryptrIdent); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    indirectbr i8* %s, [", nextStmtIdent); err != nil {
			return err
		}
		for i := range statements {
			if _, err := fmt.Fprintf(w, "label %%stmt%d,", i); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(w, "label %%stmt%d]\n", len(statements)); err != nil {
			return err
		}

	case StatementIgnore, StatementRemember:
		flag := 1
		if stmt.Type == StatementIgnore {
			if _, err := fmt.Fprintf(w, "    ;IGNORE\n"); err != nil {
				return err
			}
		} else {
			flag = 0
			if _, err := fmt.Fprintf(w, "    ;REMEMBER\n"); err != nil {
				return err
			}
		}
		for _, operand := range stmt.Operands.([]Stashable) {
			switch v := operand.(type) {
			case Var16:
				if _, err := fmt.Fprintf(w, "    store i1 %d,i1* getelementptr(%%vrbl, %%vrbl* @onespot%d,i32 0,i32 0)\n", flag, v); err != nil {
					return err
				}
			case Var32:
				if _, err := fmt.Fprintf(w, "    store i1 %d,i1* getelementptr(%%vrbl, %%vrbl* @twospot%d,i32 0,i32 0)\n", flag, v); err != nil {
					return err
				}
			case Array16:
				if _, err := fmt.Fprintf(w, "    store i1 %d,i1* getelementptr(%%vrbl, %%arr_vrbl* @tail%d,i32 0,i32 0)\n", flag, v); err != nil {
					return err
				}
			case Array32:
				if _, err := fmt.Fprintf(w, "    store i1 %d,i1* getelementptr(%%vrbl, %%arr_vrbl* @hybrid%d,i32 0,i32 0)\n", flag, v); err != nil {
					return err
				}
			default:
				panic("Ignore/Remember")
			}
		}
		if _, err := fmt.Fprintf(w, "    br label %%%s\n  %s:\n    br label %%stmt%d\n", redoLabel, redoDoneLabel, nextIndex(stmt, statements)); err != nil {
			return err
		}

	case StatementAbstainLabel, StatementAbstainGerundList, StatementReinstateLabel, StatementReinstateGerundList:
		flag := 1
		if stmt.Type == StatementReinstateLabel || stmt.Type == StatementReinstateGerundList {
			flag = 0
			if _, err := fmt.Fprintf(w, "    ;REINSTATE\n"); err != nil {
				return err
			}
		} else {
			if _, err := fmt.Fprintf(w, "    ;ABSTAIN\n"); err != nil {
				return err
			}
		}
		for _, index := range stmt.Operands.([]int) {
			if _, err := fmt.Fprintf(w, "    store i1 %d, i1* getelementptr([%d x i1], [%d x i1]* @abstain_flags,i32 0,i32 %d)\n", flag, len(statements)+1, len(statements)+1, index); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(w, "    br label %%%s\n  %s:\n    br label %%stmt%d\n", redoLabel, redoDoneLabel, stmt.Index+1); err != nil {
			return err
		}

	case StatementGiveUp:
		if _, err := fmt.Fprintf(w, "    ;GIVE UP\n    call void @exit(i32 0) noreturn\n    br label %%%s\n", redoLabel); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "  %s:\n    call void @exit(i32 0) noreturn\n    br label %%stmt%d\n", redoDoneLabel, stmt.Index+1); err != nil {
			return err
		}

	case StatementReadOut:
		if _, err := fmt.Fprintf(w, "    ;READ OUT\n"); err != nil {
			return err
		}
		for _, arg := range stmt.Operands.([]ReadOutable) {
			newLabelCounter, exprIdent, err := codeGenExpr(w, stmt, arg, labelCounter)
			if err != nil {
				return err
			}
			labelCounter = newLabelCounter
			if _, err := fmt.Fprintf(w, "    call void @check_error(%%val %s, i32 %d)\n", exprIdent, nextIndex(stmt, statements)); err != nil {
				return err
			}
			valIdent := fmt.Sprintf("%%val%d.%d", stmt.Index, labelCounter)
			labelCounter++
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s, 1\n    call void @output(i32 %s)\n", valIdent, exprIdent, valIdent); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(w, "    br label %%%s\n", redoLabel); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "  %s:\n    br label %%stmt%d\n", redoDoneLabel, nextIndex(stmt, statements)); err != nil {
			return err
		}

	default:
		if _, err := fmt.Fprintf(w, "    ;UNKNOWN ERROR\n    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 778,1),i32 %d)\n    br label %%%s\n", stmt.Index+1, redoLabel); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "  %s:\n    call void @exit(i32 1) noreturn\n    br label %%stmt%d\n", redoDoneLabel, stmt.Index+1); err != nil {
			return err
		}
	}
	return nil
}

func codeGenExpr(w io.Writer, stmt *Statement, expr Expr, labelCounter int) (int, string, error) {
	if val, is16, isConst := expr.ConstValue(); isConst {
		ident := fmt.Sprintf("%%expr.%d.%d", stmt.Index, labelCounter)
		labelCounter++
		tag := 0
		if !is16 {
			tag = 1
		}
		if _, err := fmt.Fprintf(w, "    %s = select i1 1, %%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 %d,0),i32 %d,1), %%val zeroinitializer\n", ident, tag, val); err != nil {
			return 0, "", nil
		}
		return labelCounter, ident, nil
	}

	ident := fmt.Sprintf("%%expr.%d.%d", stmt.Index, labelCounter)
	labelCounter++
	switch e := expr.(type) {
	case Var16:
		//...

	case Var32:
		//...

	case ArrayElement:
		//...

	case ExprConst:
		panic("ExprConst")

	case ExprMingle:
		labelCounter2, leftIdent, err := codeGenExpr(w, stmt, e[0], labelCounter)
		if err != nil {
			return 0, "", err
		}
		labelCounter3, rightIdent, err := codeGenExpr(w, stmt, e[1], labelCounter2)
		if err != nil {
			return 0, "", err
		}
		if _, err := fmt.Fprintf(w, "    %s = call %%val @op_mingle(%%val %s,%%val %s)\n", ident, leftIdent, rightIdent); err != nil {
			return 0, "", err
		}
		return labelCounter3, ident, nil

	case ExprSelect:
		labelCounter2, leftIdent, err := codeGenExpr(w, stmt, e[0], labelCounter)
		if err != nil {
			return 0, "", err
		}
		labelCounter3, rightIdent, err := codeGenExpr(w, stmt, e[1], labelCounter2)
		if err != nil {
			return 0, "", err
		}
		if _, err := fmt.Fprintf(w, "    %s = call %%val @op_select(%%val %s,%%val %s)\n", ident, leftIdent, rightIdent); err != nil {
			return 0, "", err
		}
		return labelCounter3, ident, nil

	case ExprAnd:
		newLabelCounter, operandIdent, err := codeGenExpr(w, stmt, e[0], labelCounter)
		if err != nil {
			return 0, "", err
		}
		if _, err := fmt.Fprintf(w, "    %s = call %%val @op_and(%%val %s)\n", ident, operandIdent); err != nil {
			return 0, "", err
		}
		return newLabelCounter, ident, nil

	case ExprOr:
		newLabelCounter, operandIdent, err := codeGenExpr(w, stmt, e[0], labelCounter)
		if err != nil {
			return 0, "", err
		}
		if _, err := fmt.Fprintf(w, "    %s = call %%val @op_or(%%val %s)\n", ident, operandIdent); err != nil {
			return 0, "", err
		}
		return newLabelCounter, ident, nil

	case ExprXor:
		newLabelCounter, operandIdent, err := codeGenExpr(w, stmt, e[0], labelCounter)
		if err != nil {
			return 0, "", err
		}
		if _, err := fmt.Fprintf(w, "    %s = call %%val @op_xor(%%val %s)\n", ident, operandIdent); err != nil {
			return 0, "", err
		}
		return newLabelCounter, ident, nil

	default:
		panic("Expr")
	}
	if _, err := fmt.Fprintf(w, "    %s = select i1 1, %%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 778,1), %%val zeroinitializer\n", ident); err != nil {
		return 0, "", nil
	}
	return labelCounter, ident, nil
}

func codeGenVariables(w io.Writer, statements []*Statement) error {
	onespots := make(map[Var16]bool)
	twospots := make(map[Var32]bool)
	tails := make(map[Array16]bool)
	hybrids := make(map[Array32]bool)
	for _, stmt := range statements {
		collectStmtVariables(stmt, onespots, twospots, tails, hybrids)
	}
	for v, _ := range onespots {
		if _, err := fmt.Fprintf(w, "@onespot%d = global %%vrbl zeroinitializer\n", v); err != nil {
			return err
		}
	}
	for v, _ := range twospots {
		if _, err := fmt.Fprintf(w, "@twospot%d = global %%vrbl zeroinitializer\n", v); err != nil {
			return err
		}
	}
	for v, _ := range tails {
		if _, err := fmt.Fprintf(w, "@tail%d = global %%arr_vrbl zeroinitializer\n", v); err != nil {
			return err
		}
	}
	for v, _ := range hybrids {
		if _, err := fmt.Fprintf(w, "@hybrid%d = global %%arr_vrbl zeroinitializer\n", v); err != nil {
			return err
		}
	}
	return nil
}

func collectStmtVariables(stmt *Statement, onespots map[Var16]bool, twospots map[Var32]bool, tails map[Array16]bool, hybrids map[Array32]bool) {
	switch stmt.Type {
	case StatementCalculate:
		calculation := stmt.Operands.(Calculation)
		switch lhs := calculation.LHS.(type) {
		case Var16:
			onespots[lhs] = true
		case Var32:
			twospots[lhs] = true
		case ArrayElement:
			switch arr := lhs.Array.(type) {
			case Array16:
				tails[arr] = true
			case Array32:
				hybrids[arr] = true
			default:
				panic("Calculate")
			}
			for _, expr := range lhs.Index {
				collectExprVariables(expr, onespots, twospots, tails, hybrids)
			}
		default:
			panic("Calculate")
		}
		collectExprVariables(calculation.RHS, onespots, twospots, tails, hybrids)
	case StatementCalculateArrayDimension:
		dim := stmt.Operands.(Dimensioning)
		switch lhs := dim.LHS.(type) {
		case Array16:
			tails[lhs] = true
		case Array32:
			hybrids[lhs] = true
		default:
			panic("ArrayDimension")
		}
		for _, expr := range dim.RHS {
			collectExprVariables(expr, onespots, twospots, tails, hybrids)
		}
	case StatementForget, StatementResume:
		collectExprVariables(stmt.Operands.(Expr), onespots, twospots, tails, hybrids)
	case StatementStash, StatementRetrieve, StatementIgnore, StatementRemember:
		for _, arg := range stmt.Operands.([]Stashable) {
			switch v := arg.(type) {
			case Var16:
				onespots[v] = true
			case Var32:
				twospots[v] = true
			case Array16:
				tails[v] = true
			case Array32:
				hybrids[v] = true
			default:
				panic("Stash")
			}
		}
	case StatementWriteIn:
		for _, arg := range stmt.Operands.([]WriteInable) {
			switch v := arg.(type) {
			case Var16:
				onespots[v] = true
			case Var32:
				twospots[v] = true
			case ArrayElement:
				switch arr := v.Array.(type) {
				case Array16:
					tails[arr] = true
				case Array32:
					hybrids[arr] = true
				default:
					panic("WriteIn")
				}
				for _, expr := range v.Index {
					collectExprVariables(expr, onespots, twospots, tails, hybrids)
				}
			case Array16:
				tails[v] = true
			case Array32:
				hybrids[v] = true
			default:
				panic("WriteIn")
			}
		}
	case StatementReadOut:
		for _, arg := range stmt.Operands.([]ReadOutable) {
			switch v := arg.(type) {
			case Var16:
				onespots[v] = true
			case Var32:
				twospots[v] = true
			case ArrayElement:
				switch arr := v.Array.(type) {
				case Array16:
					tails[arr] = true
				case Array32:
					hybrids[arr] = true
				default:
					panic("ReadOut")
				}
				for _, expr := range v.Index {
					collectExprVariables(expr, onespots, twospots, tails, hybrids)
				}
			case ExprConst:
			default:
				panic("ReadOut")
			}
		}
	}
}

func collectExprVariables(expr Expr, onespots map[Var16]bool, twospots map[Var32]bool, tails map[Array16]bool, hybrids map[Array32]bool) {
	switch e := expr.(type) {
	case Var16:
		onespots[e] = true
	case Var32:
		twospots[e] = true
	case ArrayElement:
		switch arr := e.Array.(type) {
		case Array16:
			tails[arr] = true
		case Array32:
			hybrids[arr] = true
		default:
			panic("Expr")
		}
		for _, expr := range e.Index {
			collectExprVariables(expr, onespots, twospots, tails, hybrids)
		}
	case ExprConst:
	case ExprMingle:
		collectExprVariables(e[0], onespots, twospots, tails, hybrids)
		collectExprVariables(e[1], onespots, twospots, tails, hybrids)
	case ExprSelect:
		collectExprVariables(e[0], onespots, twospots, tails, hybrids)
		collectExprVariables(e[1], onespots, twospots, tails, hybrids)
	case ExprAnd:
		collectExprVariables(e[0], onespots, twospots, tails, hybrids)
	case ExprOr:
		collectExprVariables(e[0], onespots, twospots, tails, hybrids)
	case ExprXor:
		collectExprVariables(e[0], onespots, twospots, tails, hybrids)
	}
}
