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

	mainSubprogramDebugInfo, debugLocations, err := codeGenDebugLocations(w, statements)
	if err != nil {
		return err
	}

	// globals for variables and arrays
	if err := codeGenVariables(w, statements); err != nil {
		return err
	}

	// void @main()
	if _, err := fmt.Fprintf(w, "define void @main()%s {\n    call i32 @write(i32 2, i8* getelementptr([%d x i8],[%d x i8]* @program_listing,i32 0,i32 0),i32 %d)\n    %%random_seed = call i32 @time(i8* null)\n    call void @srandom(i32 %%random_seed)\n    br label %%stmt0\n", mainSubprogramDebugInfo, listingSize+4, listingSize+4, listingSize); err != nil {
		return err
	}

	for i, stmt := range statements {
		if err := codeGenStmt(w, stmt, statements, listingIndexes[i], listingSize, debugLocations[i]); err != nil {
			return err
		}
	}

	if _, err := fmt.Fprintf(w, "  stmt%d:\n    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 633,1),i32 %d) noreturn\n    ret void\n}\n", len(statements), len(statements)); err != nil {
		return err
	}

	return nil
}

func codeGenDebugLocations(w io.Writer, statements []*Statement) (string, []string, error) {
	hasLocations := false
	for _, stmt := range statements {
		var stmtToken *Token
		if stmt.Label == 0 && len(stmt.Tokens) > 0 {
			stmtToken = stmt.Tokens[0]
		} else if stmt.Label > 0 && len(stmt.Tokens) > 3 {
			stmtToken = stmt.Tokens[3]
		}
		if stmtToken != nil && stmtToken.Location.Filename != "" {
			hasLocations = true
			break
		}
	}
	if !hasLocations {
		return "", make([]string, len(statements)), nil
	}

	if _, err := fmt.Fprintf(w, "!llvm.module.flags = !{!0, !1}\n"); err != nil {
		return "", nil, err
	}
	if _, err := fmt.Fprintf(w, "!0 = !{i32 2, !\"Dwarf Version\", i32 4}\n"); err != nil {
		return "", nil, err
	}
	if _, err := fmt.Fprintf(w, "!1 = !{i32 2, !\"Debug Info Version\", i32 3}\n"); err != nil {
		return "", nil, err
	}

	mainSubprogramDebugInfo := ""
	metainfoCounter := 2
	files := make(map[string]int)
	compileUnits := []int{}
	for _, stmt := range statements {
		var stmtToken *Token
		if stmt.Label == 0 && len(stmt.Tokens) > 0 {
			stmtToken = stmt.Tokens[0]
		} else if stmt.Label > 0 && len(stmt.Tokens) > 3 {
			stmtToken = stmt.Tokens[3]
		}
		if stmtToken == nil || stmtToken.Location.Filename == "" {
			continue
		}
		if _, ok := files[stmtToken.Location.Filename]; ok {
			continue
		}
		if _, err := fmt.Fprintf(w, "!%d = !DIFile(filename: \"%s\", directory: \"%s\")\n", metainfoCounter, stmtToken.Location.Filename, stmtToken.Location.Dir); err != nil {
			return "", nil, err
		}
		if _, err := fmt.Fprintf(w, "!%d = distinct !DICompileUnit(language: DW_LANG_C89, file: !%d, emissionKind: FullDebug)\n", metainfoCounter+1, metainfoCounter); err != nil {
			return "", nil, err
		}
		if len(files) == 0 {
			if _, err := fmt.Fprintf(w, "!%d = distinct !DISubprogram(name: \"PROGRAM\", linkageName: \"main\", file: !%d, unit: !%d, type: !%d)\n", metainfoCounter+2, metainfoCounter, metainfoCounter+1, metainfoCounter+3); err != nil {
				return "", nil, err
			}
		} else {
			if _, err := fmt.Fprintf(w, "!%d = distinct !DISubprogram(file: !%d, unit: !%d, type: !%d)\n", metainfoCounter+2, metainfoCounter, metainfoCounter+1, metainfoCounter+3); err != nil {
				return "", nil, err
			}
		}
		if _, err := fmt.Fprintf(w, "!%d = !DISubroutineType(types: !{})\n", metainfoCounter+3); err != nil {
			return "", nil, err
		}
		files[stmtToken.Location.Filename] = metainfoCounter + 2
		compileUnits = append(compileUnits, metainfoCounter+1)
		if mainSubprogramDebugInfo == "" {
			mainSubprogramDebugInfo = fmt.Sprintf(" !dbg !%d", metainfoCounter+2)
		}
		metainfoCounter += 4
	}

	if _, err := fmt.Fprintf(w, "!llvm.dbg.cu = !{"); err != nil {
		return "", nil, err
	}
	for i, cu := range compileUnits {
		comma := ","
		if i == 0 {
			comma = ""
		}
		if _, err := fmt.Fprintf(w, "%s!%d", comma, cu); err != nil {
			return "", nil, err
		}
	}
	if _, err := fmt.Fprintf(w, "}\n"); err != nil {
		return "", nil, err
	}

	debugLocations := []string{}
	for _, stmt := range statements {
		var stmtToken *Token
		if stmt.Label == 0 && len(stmt.Tokens) > 0 {
			stmtToken = stmt.Tokens[0]
		} else if stmt.Label > 0 && len(stmt.Tokens) > 3 {
			stmtToken = stmt.Tokens[3]
		}
		if stmtToken == nil || stmtToken.Location.Filename == "" {
			debugLocations = append(debugLocations, "")
		} else {
			if _, err := fmt.Fprintf(w, "!%d = !DILocation(line: %d, column: %d, scope: !%d)\n", metainfoCounter, stmtToken.Location.Line, stmtToken.Location.Column, files[stmtToken.Location.Filename]); err != nil {
				return "", nil, err
			}
			debugLocations = append(debugLocations, fmt.Sprintf(", !dbg !%d", metainfoCounter))
			metainfoCounter++
		}
	}
	return mainSubprogramDebugInfo, debugLocations, nil
}

func nextIndex(stmt *Statement, statements []*Statement) int {
	switch stmt.Type {
	case StatementNext:
		if stmt.Operands != nil {
			return stmt.Operands.(int)
		}
	case StatementResume:
		return len(statements)
	}
	return stmt.Index + 1
}

func codeGenStmt(w io.Writer, stmt *Statement, statements []*Statement, listingIndexes [2]int, listingSize int, debugLocation string) error {
	if _, err := fmt.Fprintf(w, "  stmt%d:\n", stmt.Index); err != nil {
		return err
	}
	if stmt.Label != 0 && len(stmt.Tokens) > 3 {
		if _, err := fmt.Fprintf(w, "    ; %s:%d:%d\n", stmt.Tokens[3].Location.Filename, stmt.Tokens[3].Location.Line, stmt.Tokens[3].Location.Column); err != nil {
			return err
		}
	} else if len(stmt.Tokens) > 0 {
		if _, err := fmt.Fprintf(w, "    ; %s:%d:%d\n", stmt.Tokens[0].Location.Filename, stmt.Tokens[0].Location.Line, stmt.Tokens[0].Location.Column); err != nil {
			return err
		}
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
			if _, err := fmt.Fprintf(w, "    %s = call i1 @random_check(i32 9)%s\n", ident1, debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    br i1 %s, label %%%s, label %%%s%s\n", ident1, label1, label2, debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "  %s:\n    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 774,1),i32 %d) noreturn%s\n", label1, nextIndex(stmt, statements), debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    ret void%s\n  %s:\n", debugLocation, label2); err != nil {
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
		if _, err := fmt.Fprintf(w, "    %s = getelementptr [%d x i1], [%d x i1]* @abstain_flags,i32 0,i32 %d%s\n    %s = load i1, i1* %s%s\n    br i1 %s, label %%stmt%d, label %%%s%s\n  %s:\n", ident1, len(statements)+1, len(statements)+1, stmt.Index, debugLocation, ident2, ident1, debugLocation, ident2, stmt.Index+1, label1, debugLocation, label1); err != nil {
			return err
		}
	}

	if stmt.Chance == 0 {
		if _, err := fmt.Fprintf(w, "    br label %%stmt%d%s\n", stmt.Index+1, debugLocation); err != nil {
			return err
		}
	}

	if stmt.Error != nil && stmt.Error.Message() != "" {
		if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 %d,1),i32 %d) noreturn%s\n    br label %%stmt%d%s\n", stmt.Error.Code(), stmt.Index+1, debugLocation, stmt.Index+1, debugLocation); err != nil {
			return err
		}
		return nil
	}

	if stmt.Type == StatementUnrecognizable || (stmt.Error != nil && stmt.Error.Code() == 0) {
		if _, err := fmt.Fprintf(w, "    ;SYNTAX ERROR\n    call i32 @write(i32 2, i8* getelementptr([8 x i8], [8 x i8]* @error_message_000_prefix,i32 0,i32 0),i32 8)%s\n    call i32 @write(i32 2, i8* getelementptr([%d x i8], [%d x i8]* @program_listing,i32 0,i32 %d),i32 %d)%s\n", debugLocation, listingSize+4, listingSize+4, listingIndexes[0], listingIndexes[1], debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 3,0),i32 0,1),i32 %d) noreturn%s\n    br label %%stmt%d%s\n", stmt.Index+1, debugLocation, stmt.Index+1, debugLocation); err != nil {
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
		if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n    br label %%%s%s\n  %s:\n    br label %%%s%s\n  %s:\n", doLabel, debugLocation, doLabel, countCheckLabel, debugLocation, redoLabel, countCheckLabel, debugLocation, countCheckLabel); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    %s = phi i32 [0,%%%s], [%s,%%%s]%s\n", countIdent, doLabel, nextCountIdent, redoLabel, debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    %s = add i32 %s, 1%s\n", nextCountIdent, countIdent, debugLocation); err != nil {
			return err
		}

		countLimitIdent := fmt.Sprintf("%%countcheck%d.%d", stmt.Index, labelCounter)
		randomCheckLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+1)
		labelCounter += 2
		if _, err := fmt.Fprintf(w, "    %s = icmp ult i32 %s, %d%s\n    br i1 %s, label %%%s, label %%%s%s\n  %s:\n", countLimitIdent, countIdent, stmt.Chance/100, debugLocation, countLimitIdent, startStmtLabel, randomCheckLabel, debugLocation, randomCheckLabel); err != nil {
			return err
		}

		if stmt.Chance%100 == 0 {
			if _, err := fmt.Fprintf(w, "    br label %%%s%s\n", redoDoneLabel, debugLocation); err != nil {
				return err
			}
		} else {
			issecondRandomCheckIdent := fmt.Sprintf("%%issecondrandomcheck%d.%d", stmt.Index, labelCounter)
			firstRandomCheckLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter)
			labelCounter += 2
			if _, err := fmt.Fprintf(w, "    %s = icmp ugt i32 %s, %d%s\n    br i1 %s, label %%%s, label %%%s%s\n  %s:\n", issecondRandomCheckIdent, countIdent, stmt.Chance/100, debugLocation, issecondRandomCheckIdent, redoDoneLabel, firstRandomCheckLabel, debugLocation, firstRandomCheckLabel); err != nil {
				return err
			}
			randomCheckResult := fmt.Sprintf("%%randomcheck%d.%d", stmt.Index, labelCounter)
			labelCounter++
			doneLabel := redoDoneLabel
			if stmt.Chance <= 100 {
				doneLabel = fmt.Sprintf("stmt%d", stmt.Index+1)
			}
			if _, err := fmt.Fprintf(w, "    %s = call i1 @random_check(i32 %d)%s\n    br i1 %s, label %%%s, label %%%s%s\n", randomCheckResult, 10*(stmt.Chance%100), debugLocation, randomCheckResult, startStmtLabel, doneLabel, debugLocation); err != nil {
			}
		}
		if _, err := fmt.Fprintf(w, "  %s:\n", startStmtLabel); err != nil {
			return err
		}
	}

	if stmt.Error != nil {
		if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 %d,1),i32 %d)%s\n    br label %%stmt%d%s\n", stmt.Error.Code(), nextIndex(stmt, statements), debugLocation, nextIndex(stmt, statements), debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "  %s:\n    br label %%stmt%d\n", redoDoneLabel, nextIndex(stmt, statements)); err != nil {
			return err
		}
		return nil
	}

	switch stmt.Type {
	case StatementCalculate:
		if _, err := fmt.Fprintf(w, "    ;CALCULATE\n"); err != nil {
			return err
		}
		calc := stmt.Operands.(Calculation)
		if newLabelCounter, ignoredIdent, err := codeGenCheckIgnored(w, stmt, calc.LHS, labelCounter); err != nil {
			return err
		} else {
			labelCounter = newLabelCounter
			notignoredLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter)
			labelCounter++
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s\n  %s:\n", ignoredIdent, redoLabel, notignoredLabel, notignoredLabel); err != nil {
				return err
			}
		}
		if newLabelCounter, rhsIdent, err := codeGenExpr(w, stmt, calc.RHS, labelCounter); err != nil {
			return err
		} else if newNewLabelCounter, err := codeGenGets(w, stmt, calc.LHS, rhsIdent, redoLabel, newLabelCounter); err != nil {
			return err
		} else {
			labelCounter = newNewLabelCounter
		}
		if _, err := fmt.Fprintf(w, "  %s:\n    br label %%stmt%d\n", redoDoneLabel, nextIndex(stmt, statements)); err != nil {
			return err
		}

	case StatementCalculateArrayDimension:
		if _, err := fmt.Fprintf(w, "    ;CALCULATE\n"); err != nil {
			return err
		}
		dim := stmt.Operands.(Dimensioning)
		if newLabelCounter, ignoredIdent, err := codeGenCheckIgnored(w, stmt, dim.LHS, labelCounter); err != nil {
			return err
		} else {
			labelCounter = newLabelCounter
			notignoredLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter)
			labelCounter++
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s\n  %s:\n", ignoredIdent, redoLabel, notignoredLabel, notignoredLabel); err != nil {
				return err
			}
		}

		totaldimIdent := ""
		dimIdents := []string{}
		for i, dimExpr := range dim.RHS {
			newLabelCounter, dimexprIdent, err := codeGenExpr(w, stmt, dimExpr, labelCounter)
			if err != nil {
				return err
			}
			labelCounter = newLabelCounter
			if _, err := fmt.Fprintf(w, "    call void @check_error(%%val %s,i32 %d)\n", dimexprIdent, nextIndex(stmt, statements)); err != nil {
				return err
			}

			dimIdent := fmt.Sprintf("%%dim%d.%d", stmt.Index, labelCounter)
			dimiszeroIdent := fmt.Sprintf("%%dimiszero%d.%d", stmt.Index, labelCounter)
			labelCounter += 2
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1\n", dimIdent, dimexprIdent); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    %s = icmp eq i32 %s,0\n", dimiszeroIdent, dimIdent); err != nil {
				return err
			}

			dimiszeroLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter)
			dimisokLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+1)
			labelCounter += 2
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s\n", dimiszeroIdent, dimiszeroLabel, dimisokLabel); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "  %s:\n    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 240,1),i32 %d)\n    br label %%%s\n", dimiszeroLabel, nextIndex(stmt, statements), redoLabel); err != nil {
				return err
			}

			if _, err := fmt.Fprintf(w, "  %s:\n", dimisokLabel); err != nil {
				return err
			}

			dimIdents = append(dimIdents, dimIdent)
			if i == 0 {
				totaldimIdent = dimIdent
			} else {
				newtotaldimIdent := fmt.Sprintf("%%totaldim%d.%d", stmt.Index, labelCounter)
				labelCounter++
				if _, err := fmt.Fprintf(w, "    %s = mul i32 %s,%s\n", newtotaldimIdent, totaldimIdent, dimIdent); err != nil {
					return err
				}
				totaldimIdent = newtotaldimIdent
			}
		}

		i32arrsizeIdent := fmt.Sprintf("%%i32arrsize%d.%d", stmt.Index, labelCounter)
		mallocsizeaddrIdent := fmt.Sprintf("%%mallocsizeaddr%d.%d", stmt.Index, labelCounter+1)
		mallocsizeIdent := fmt.Sprintf("%%mallocsize%d.%d", stmt.Index, labelCounter+2)
		labelCounter += 3
		if _, err := fmt.Fprintf(w, "    %s = add i32 %d,%s\n", i32arrsizeIdent, 1+len(dim.RHS), totaldimIdent); err != nil {
			return nil
		}
		if _, err := fmt.Fprintf(w, "    %s = getelementptr %%arr_val,%%arr_val* null,i32 0,i32 1,i32 %s\n", mallocsizeaddrIdent, i32arrsizeIdent); err != nil {
			return nil
		}
		if _, err := fmt.Fprintf(w, "    %s = ptrtoint i32* %s to i32\n", mallocsizeIdent, mallocsizeaddrIdent); err != nil {
			return nil
		}

		mallocresultIdent := fmt.Sprintf("%%mallocresult%d.%d", stmt.Index, labelCounter)
		newarrvalIdent := fmt.Sprintf("%%newarrval%d.%d", stmt.Index, labelCounter+1)
		newarrvaldimptrIdent := fmt.Sprintf("%%newarrvaldim%d.%d", stmt.Index, labelCounter+2)
		labelCounter += 3
		if _, err := fmt.Fprintf(w, "    %s = call i8* @malloc(i32 %s)\n", mallocresultIdent, mallocsizeIdent); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    call void @llvm.memset.p0i8.i32(i8* %s,i8 0,i32 %s,i32 0,i1 0)\n", mallocresultIdent, mallocsizeIdent); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    %s = bitcast i8* %s to %%arr_val*\n", newarrvalIdent, mallocresultIdent); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    %s = getelementptr %%arr_val,%%arr_val* %s,i32 0,i32 1,i32 0\n    store i32 %d,i32* %s\n", newarrvaldimptrIdent, newarrvalIdent, len(dim.RHS), newarrvaldimptrIdent); err != nil {
			return err
		}
		for i, dimIdent := range dimIdents {
			newdimptrIdent := fmt.Sprintf("%%newdimptr%d.%d", stmt.Index, labelCounter)
			labelCounter++
			if _, err := fmt.Fprintf(w, "    %s = getelementptr %%arr_val,%%arr_val* %s,i32 0,i32 1,i32 %d\n", newdimptrIdent, newarrvalIdent, i+1); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    store i32 %s,i32* %s\n", dimIdent, newdimptrIdent); err != nil {
				return err
			}
		}

		arrvalptrIdent := fmt.Sprintf("%%arrvalptr%d.%d", stmt.Index, labelCounter)
		labelCounter++
		switch arr := dim.LHS.(type) {
		case Array16:
			if _, err := fmt.Fprintf(w, "    %s = getelementptr %%arr_vrbl,%%arr_vrbl* @tail%d,i32 0,i32 1\n", arrvalptrIdent, arr); err != nil {
				return err
			}
		case Array32:
			if _, err := fmt.Fprintf(w, "    %s = getelementptr %%arr_vrbl,%%arr_vrbl* @hybrid%d,i32 0,i32 1\n", arrvalptrIdent, arr); err != nil {
				return err
			}
		default:
			panic("Calculate Array Dimensions")
		}

		oldarrvalIdent := fmt.Sprintf("%%oldarrval%d.%d", stmt.Index, labelCounter)
		oldarrvalisnullIdent := fmt.Sprintf("%%oldarrvalisnull%d.%d", stmt.Index, labelCounter+1)
		freeoldarrvalLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+2)
		storenewarrvalLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+3)
		labelCounter += 4
		if _, err := fmt.Fprintf(w, "    %s = load %%arr_val*,%%arr_val** %s\n", oldarrvalIdent, arrvalptrIdent); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    %s = icmp eq %%arr_val* %s,null\n", oldarrvalisnullIdent, oldarrvalIdent); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s\n", oldarrvalisnullIdent, storenewarrvalLabel, freeoldarrvalLabel); err != nil {
			return err
		}

		oldstashptrIdent := fmt.Sprintf("%%oldstashptr%d.%d", stmt.Index, labelCounter)
		oldstashIdent := fmt.Sprintf("%%oldstash%d.%d", stmt.Index, labelCounter+1)
		newstashptrIdent := fmt.Sprintf("%%newstashptr%d.%d", stmt.Index, labelCounter+2)
		oldarrvali8ptrIdent := fmt.Sprintf("%%oldarrvali8ptr%d.%d", stmt.Index, labelCounter+3)
		labelCounter += 4
		if _, err := fmt.Fprintf(w, "  %s:\n    %s = getelementptr %%arr_val,%%arr_val* %s,i32 0,i32 0", freeoldarrvalLabel, oldstashptrIdent, oldarrvalIdent); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    %s = load %%arr_val*,%%arr_val** %s", oldstashIdent, oldstashptrIdent); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    %s = getelementptr %%arr_val,%%arr_val* %s,i32 0,i32 0", newstashptrIdent, newarrvalIdent); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    store %%arr_val* %s,%%arr_val** %s", oldstashIdent, newstashptrIdent); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    %s = bitcast %%arr_val* %s to i8*\n", oldarrvali8ptrIdent, oldarrvalIdent); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    call void @free(i8* %s)\n", oldarrvali8ptrIdent); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    br label %%%s\n", storenewarrvalLabel); err != nil {
			return err
		}

		if _, err := fmt.Fprintf(w, "  %s:\n    store %%arr_val* %s,%%arr_val** %s\n", storenewarrvalLabel, newarrvalIdent, arrvalptrIdent); err != nil {
			return err
		}

		if _, err := fmt.Fprintf(w, "    br label %%%s\n", redoLabel); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "  %s:\n    br label %%stmt%d\n", redoDoneLabel, nextIndex(stmt, statements)); err != nil {
			return err
		}

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

	case StatementStash:
		if _, err := fmt.Fprintf(w, "    ;STASH\n"); err != nil {
			return err
		}
		for _, operand := range stmt.Operands.([]Stashable) {
			notignoredLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter)
			nextLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+1)
			labelCounter += 2
			if newLabelCounter, ignoredIdent, err := codeGenCheckIgnored(w, stmt, operand, labelCounter); err != nil {
				return err
			} else {
				labelCounter = newLabelCounter
				if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s\n  %s:\n", ignoredIdent, nextLabel, notignoredLabel, notignoredLabel); err != nil {
					return err
				}
			}

			switch v := operand.(type) {
			case Var16:
				if _, err := fmt.Fprintf(w, "    call void @stash_var(%%vrbl* @onespot%d)\n", v); err != nil {
					return err
				}
			case Var32:
				if _, err := fmt.Fprintf(w, "    call void @stash_var(%%vrbl* @twospot%d)\n", v); err != nil {
					return err
				}
			case Array16:
				if _, err := fmt.Fprintf(w, "    call void @stash_arr(%%arr_vrbl* @tail%d)\n", v); err != nil {
					return err
				}
			case Array32:
				if _, err := fmt.Fprintf(w, "    call void @stash_arr(%%arr_vrbl* @hybrid%d)\n", v); err != nil {
					return err
				}
			default:
				panic("Stash")
			}

			if _, err := fmt.Fprintf(w, "    br label %%%s\n  %s:\n", nextLabel, nextLabel); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(w, "    br label %%%s\n  %s:\n    br label %%stmt%d\n", redoLabel, redoDoneLabel, nextIndex(stmt, statements)); err != nil {
			return err
		}

	case StatementRetrieve:
		if _, err := fmt.Fprintf(w, "    ;RETRIEVE\n"); err != nil {
			return err
		}
		for _, operand := range stmt.Operands.([]Stashable) {
			notignoredLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter)
			nextLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+1)
			labelCounter += 2
			if newLabelCounter, ignoredIdent, err := codeGenCheckIgnored(w, stmt, operand, labelCounter); err != nil {
				return err
			} else {
				labelCounter = newLabelCounter
				if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s\n  %s:\n", ignoredIdent, nextLabel, notignoredLabel, notignoredLabel); err != nil {
					return err
				}
			}

			retrievesucceededIdent := fmt.Sprintf("%%retrievesucceeded%d.%d", stmt.Index, labelCounter)
			labelCounter++

			switch v := operand.(type) {
			case Var16:
				if _, err := fmt.Fprintf(w, "    %s = call i1 @retrieve_var(%%vrbl* @onespot%d)\n", retrievesucceededIdent, v); err != nil {
					return err
				}
			case Var32:
				if _, err := fmt.Fprintf(w, "    %s = call i1 @retrieve_var(%%vrbl* @twospot%d)\n", retrievesucceededIdent, v); err != nil {
					return err
				}
			case Array16:
				if _, err := fmt.Fprintf(w, "    %s = call i1 @retrieve_arr(%%arr_vrbl* @tail%d)\n", retrievesucceededIdent, v); err != nil {
					return err
				}
			case Array32:
				if _, err := fmt.Fprintf(w, "    %s = call i1 @retrieve_arr(%%arr_vrbl* @hybrid%d)\n", retrievesucceededIdent, v); err != nil {
					return err
				}
			default:
				panic("Retrieve")
			}

			retrievefailedLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter)
			labelCounter++
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s\n", retrievesucceededIdent, nextLabel, retrievefailedLabel); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "  %s:\n    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 436,1),i32 %d)\n", retrievefailedLabel, nextIndex(stmt, statements)); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    br label %%%s\n  %s:\n", nextLabel, nextLabel); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(w, "    br label %%%s\n  %s:\n    br label %%stmt%d\n", redoLabel, redoDoneLabel, nextIndex(stmt, statements)); err != nil {
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
				if _, err := fmt.Fprintf(w, "    store i1 %d,i1* getelementptr(%%arr_vrbl, %%arr_vrbl* @tail%d,i32 0,i32 0)\n", flag, v); err != nil {
					return err
				}
			case Array32:
				if _, err := fmt.Fprintf(w, "    store i1 %d,i1* getelementptr(%%arr_vrbl, %%arr_vrbl* @hybrid%d,i32 0,i32 0)\n", flag, v); err != nil {
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

	case StatementWriteIn:
		if _, err := fmt.Fprintf(w, "    ;WRITE IN\n"); err != nil {
			return err
		}
		for _, vrbl := range stmt.Operands.([]WriteInable) {
			newLabelCounter, ignoredIdent, err := codeGenCheckIgnored(w, stmt, vrbl, labelCounter)
			if err != nil {
				return err
			}
			writeinLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, newLabelCounter)
			nextLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, newLabelCounter+1)
			labelCounter = newLabelCounter + 2
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s\n  %s:\n", ignoredIdent, nextLabel, writeinLabel, writeinLabel); err != nil {
				return err
			}

			switch v := vrbl.(type) {
			case Var16, Var32, ArrayElement:
				inputIdent := "@input32"
				if v.Is16() {
					inputIdent = "@input16"
				}
				inputvalIdent := fmt.Sprintf("%%inputval%d.%d", stmt.Index, labelCounter)
				labelCounter++
				if _, err := fmt.Fprintf(w, "    %s = call %%val %s()\n", inputvalIdent, inputIdent); err != nil {
					return err
				}

				if newLabelCounter, err := codeGenGets(w, stmt, v.(LValue), inputvalIdent, nextLabel, labelCounter); err != nil {
					return err
				} else {
					labelCounter = newLabelCounter
				}

			case Array16, Array32:
				writeinCall := ""
				if v.Is16() {
					writeinCall = fmt.Sprintf("@writein16(%%arr_vrbl* @tail%d)", v)
				} else {
					writeinCall = fmt.Sprintf("@writein32(%%arr_vrbl* @hybrid%d)", v)
				}

				errIdent := fmt.Sprintf("%%writeinerr%d.%d", stmt.Index, labelCounter)
				labelCounter++
				if _, err := fmt.Fprintf(w, "    %s = call %%val %s\n", errIdent, writeinCall); err != nil {
					return err
				}
				if _, err := fmt.Fprintf(w, "    call void @check_error(%%val %s, i32 %d)\n", errIdent, nextIndex(stmt, statements)); err != nil {
					return err
				}
				if _, err := fmt.Fprintf(w, "    br label %%%s\n", nextLabel); err != nil {
					return err
				}

			default:
				if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 778,1),i32 %d)\n    br label %s\n", nextIndex(stmt, statements), nextLabel); err != nil {
					return err
				}
			}

			if _, err := fmt.Fprintf(w, "  %s:\n", nextLabel); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(w, "    br label %%%s\n", redoLabel); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "  %s:\n    br label %%stmt%d\n", redoDoneLabel, nextIndex(stmt, statements)); err != nil {
			return err
		}

	case StatementReadOutBit:
		bit := "0"
		if stmt.Operands.(bool) {
			bit = "1"
		}
		if _, err := fmt.Fprintf(w, "    ;READ OUT\n    call void @output_binary(i1 %s)\n", bit); err != nil {
			return err
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
		ident := fmt.Sprintf("%%expr%d.%d", stmt.Index, labelCounter)
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

	ident := fmt.Sprintf("%%expr%d.%d", stmt.Index, labelCounter)
	labelCounter++
	switch e := expr.(type) {
	case Var16:
		if newLabelCounter, err := codeGenAccessVar(w, stmt, ident, fmt.Sprintf("@onespot%d", e), true, labelCounter); err != nil {
			return 0, "", err
		} else {
			return newLabelCounter, ident, nil
		}

	case Var32:
		if newLabelCounter, err := codeGenAccessVar(w, stmt, ident, fmt.Sprintf("@twospot%d", e), false, labelCounter); err != nil {
			return 0, "", err
		} else {
			return newLabelCounter, ident, nil
		}

	case ArrayElement:
		if newLabelCounter, err := codeGenAccessArrayElt(w, stmt, ident, e, labelCounter); err != nil {
			return 0, "", err
		} else {
			return newLabelCounter, ident, nil
		}

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
		error778Ident := fmt.Sprintf("%%error778_%d.%d", stmt.Index, labelCounter)
		labelCounter++
		if _, err := fmt.Fprintf(w, "    %s = select i1 1, %%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 778,1), %%val zeroinitializer\n", error778Ident); err != nil {
			return 0, "", nil
		}
		return labelCounter, error778Ident, nil
	}
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
	if stmt.Operands == nil {
		return
	}
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

func codeGenAccessVar(w io.Writer, stmt *Statement, resultIdent, varIdent string, is16 bool, labelCounter int) (int, error) {
	accessLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter)
	labelCounter++
	if _, err := fmt.Fprintf(w, "    br label %%%s\n  %s: ; access %s\n", accessLabel, accessLabel, varIdent); err != nil {
		return 0, err
	}

	valptrIdent := fmt.Sprintf("%%valptr%d.%d", stmt.Index, labelCounter)
	labelCounter++
	if _, err := fmt.Fprintf(w, "    %s = load %%vrbl_val*, %%vrbl_val** getelementptr(%%vrbl, %%vrbl* %s, i32 0, i32 1)\n", valptrIdent, varIdent); err != nil {
		return 0, err
	}

	valptrisnullIdent := fmt.Sprintf("%%valptrisnull%d.%d", stmt.Index, labelCounter)
	collectResultLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+1)
	loadResultValueLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+2)
	labelCounter += 3
	if _, err := fmt.Fprintf(w, "    %s = icmp eq %%vrbl_val* %s, null\n", valptrisnullIdent, valptrIdent); err != nil {
		return 0, err
	}
	if _, err := fmt.Fprintf(w, "    br i1 %s, label %%%s, label %%%s\n", valptrisnullIdent, collectResultLabel, loadResultValueLabel); err != nil {
		return 0, err
	}

	loadresultptrIdent := fmt.Sprintf("%%loadresultptr%d.%d", stmt.Index, labelCounter)
	loadedresultIdent := fmt.Sprintf("%%loadedresult%d.%d", stmt.Index, labelCounter+1)
	labelCounter += 2
	if _, err := fmt.Fprintf(w, "  %s:\n    %s = getelementptr %%vrbl_val, %%vrbl_val* %s,i32 0,i32 1\n", loadResultValueLabel, loadresultptrIdent, valptrIdent); err != nil {
		return 0, err
	}
	if _, err := fmt.Fprintf(w, "    %s = load i32, i32* %s\n", loadedresultIdent, loadresultptrIdent); err != nil {
		return 0, err
	}
	if _, err := fmt.Fprintf(w, "  br label %%%s\n", collectResultLabel); err != nil {
		return 0, err
	}

	collectedresultIdent := fmt.Sprintf("%%collectedresult%d.%d", stmt.Index, labelCounter)
	labelCounter++
	if _, err := fmt.Fprintf(w, "  %s:\n    %s = phi i32 [0,%%%s],[%s,%%%s]\n", collectResultLabel, collectedresultIdent, accessLabel, loadedresultIdent, loadResultValueLabel); err != nil {
		return 0, err
	}
	tag := 0
	if !is16 {
		tag = 1
	}
	if _, err := fmt.Fprintf(w, "    %s = insertvalue %%val insertvalue(%%val zeroinitializer,i2 %d,0),i32 %s,1\n", resultIdent, tag, collectedresultIdent); err != nil {
		return 0, err
	}
	return labelCounter, nil
}

func codeGenAccessArrayElt(w io.Writer, stmt *Statement, resultIdent string, arrayElement ArrayElement, labelCounter int) (int, error) {
	collectValueLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter)
	boundsErrorLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+1)
	loadvalueLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+2)
	labelCounter += 3

	newLabelCounter, indexIdentSrcLabels, arrayElementPtrIdent, err := codeGenFindArrayElement(w, stmt, boundsErrorLabel, collectValueLabel, arrayElement, labelCounter)
	if err != nil {
		return 0, err
	}
	labelCounter = newLabelCounter

	if _, err := fmt.Fprintf(w, "    br label %%%s\n  %s:\n", loadvalueLabel, loadvalueLabel); err != nil {
		return 0, err
	}
	loadedvalueIdent := fmt.Sprintf("%%loadedvalue%d.%d", stmt.Index, labelCounter)
	loadedvaluevalIdent := fmt.Sprintf("%%loadedvalueval%d.%d", stmt.Index, labelCounter+1)
	labelCounter += 2
	if _, err := fmt.Fprintf(w, "    %s = load i32,i32* %s\n", loadedvalueIdent, arrayElementPtrIdent); err != nil {
		return 0, err
	}
	tag := 0
	if _, ok := arrayElement.Array.(Array32); ok {
		tag = 1
	}
	if _, err := fmt.Fprintf(w, "    %s = insertvalue %%val insertvalue(%%val zeroinitializer,i2 %d,0),i32 %s,1\n", loadedvaluevalIdent, tag, loadedvalueIdent); err != nil {
		return 0, err
	}
	if _, err := fmt.Fprintf(w, "    br label %%%s\n", collectValueLabel); err != nil {
		return 0, err
	}

	if _, err := fmt.Fprintf(w, "  %s:\n    br label %%%s\n", boundsErrorLabel, collectValueLabel); err != nil {
		return 0, err
	}

	if _, err := fmt.Fprintf(w, "  %s:\n    %s = phi %%val [insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 241,1),%%%s]", collectValueLabel, resultIdent, boundsErrorLabel); err != nil {
		return 0, err
	}
	// errors in calculating indexes
	for _, indexIdentSrcLabel := range indexIdentSrcLabels {
		if _, err := fmt.Fprintf(w, ",[%s,%%%s]", indexIdentSrcLabel[0], indexIdentSrcLabel[1]); err != nil {
			return 0, err
		}
	}
	// result from successfully loading array element
	if _, err := fmt.Fprintf(w, ",[%s,%%%s]\n", loadedvaluevalIdent, loadvalueLabel); err != nil {
		return 0, err
	}
	return labelCounter, nil
}

// returns labelCounter, [][2]string{[2]string{indexIdent(%val),indexErrorLabel(label)}}, arrayElementPtrIdent(i32*), error
func codeGenFindArrayElement(w io.Writer, stmt *Statement, boundsErrorLabel string, indexErrorLabel string, arrayElement ArrayElement, labelCounter int) (int, [][2]string, string, error) {
	varIdent := ""
	switch arr := arrayElement.Array.(type) {
	case Array16:
		varIdent = fmt.Sprintf("@tail%d", arr)
	case Array32:
		varIdent = fmt.Sprintf("@hybrid%d", arr)
	default:
		panic("AccessArrayElement")
	}

	valptrIdent := fmt.Sprintf("%%valptr%d.%d", stmt.Index, labelCounter)
	labelCounter++
	if _, err := fmt.Fprintf(w, "    %s = load %%arr_val*, %%arr_val** getelementptr(%%arr_vrbl, %%arr_vrbl* %s, i32 0, i32 1)\n", valptrIdent, varIdent); err != nil {
		return 0, nil, "", err
	}

	// check for uninitialized array
	valptrisnullIdent := fmt.Sprintf("%%valptrisnull%d.%d", stmt.Index, labelCounter)
	checkDimsLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+1)
	labelCounter += 2
	if _, err := fmt.Fprintf(w, "    %s = icmp eq %%arr_val* %s, null\n", valptrisnullIdent, valptrIdent); err != nil {
		return 0, nil, "", err
	}
	if _, err := fmt.Fprintf(w, "    br i1 %s, label %%%s, label %%%s\n", valptrisnullIdent, boundsErrorLabel, checkDimsLabel); err != nil {
		return 0, nil, "", err
	}

	// check dimensions match
	if _, err := fmt.Fprintf(w, "  %s:\n", checkDimsLabel); err != nil {
		return 0, nil, "", err
	}
	dimsptrIdent := fmt.Sprintf("%%dimsptr%d.%d", stmt.Index, labelCounter)
	dimsIdent := fmt.Sprintf("%%dims%d.%d", stmt.Index, labelCounter+1)
	dimscheckIdent := fmt.Sprintf("%%dimscheck%d.%d", stmt.Index, labelCounter+2)
	getIndexesLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+3)
	labelCounter += 4
	if _, err := fmt.Fprintf(w, "    %s = getelementptr %%arr_val, %%arr_val* %s,i32 0,i32 1,i32 0\n", dimsptrIdent, valptrIdent); err != nil {
		return 0, nil, "", err
	}
	if _, err := fmt.Fprintf(w, "    %s = load i32, i32* %s\n", dimsIdent, dimsptrIdent); err != nil {
		return 0, nil, "", err
	}
	if _, err := fmt.Fprintf(w, "    %s = icmp eq i32 %s,%d\n", dimscheckIdent, dimsIdent, len(arrayElement.Index)); err != nil {
		return 0, nil, "", err
	}
	if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s\n", dimscheckIdent, getIndexesLabel, boundsErrorLabel); err != nil {
		return 0, nil, "", err
	}

	// get indexes
	if _, err := fmt.Fprintf(w, "  %s:\n", getIndexesLabel); err != nil {
		return 0, nil, "", err
	}
	indexIdentSrcLabels := [][2]string{} // {indexExprValIdent,label}
	for i, indexExpr := range arrayElement.Index {
		newLabelCounter, indexIdent, err := codeGenExpr(w, stmt, indexExpr, labelCounter)
		if err != nil {
			return 0, nil, "", err
		}
		indexSrcLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, newLabelCounter)
		labelCounter = newLabelCounter + 1
		indexIdentSrcLabels = append(indexIdentSrcLabels, [2]string{indexIdent, indexSrcLabel})

		if _, err := fmt.Fprintf(w, "    br label %%%s\n  %s: ;index %d error check\n", indexSrcLabel, indexSrcLabel, i); err != nil {
			return 0, nil, "", err
		}

		indextagIdent := fmt.Sprintf("%%indextag%d.%d", stmt.Index, labelCounter)
		indextagcheckIdent := fmt.Sprintf("%%indextagcheck%d.%d", stmt.Index, labelCounter)
		if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,0\n    %s = icmp uge i2 %s,2\n", indextagIdent, indexIdent, indextagcheckIdent, indextagIdent); err != nil {
			return 0, nil, "", err
		}

		nextIndexLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter)
		labelCounter++
		if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s\n  %s:\n", indextagcheckIdent, indexErrorLabel, nextIndexLabel, nextIndexLabel); err != nil {
			return 0, nil, "", err
		}
	}

	// check array bounds and calculate index to load from
	currentindextotalIdent := ""
	currentindexmultiplierIdent := ""
	for i, indexIdentSrcLabel := range indexIdentSrcLabels {
		currentindexIdent := fmt.Sprintf("%%currentindex%d.%d", stmt.Index, labelCounter)
		zerocheckIdent := fmt.Sprintf("%%zerocheck%d.%d", stmt.Index, labelCounter+1)
		zerocheckOkLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+1)
		labelCounter += 2
		if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1\n", currentindexIdent, indexIdentSrcLabel[0]); err != nil {
			return 0, nil, "", err
		}
		if _, err := fmt.Fprintf(w, "    %s = icmp eq i32 %s,0\n", zerocheckIdent, currentindexIdent); err != nil {
			return 0, nil, "", err
		}
		if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s\n  %s:\n", zerocheckIdent, boundsErrorLabel, zerocheckOkLabel, zerocheckOkLabel); err != nil {
			return 0, nil, "", err
		}

		currentindexboundptrIdent := fmt.Sprintf("%%currentindexboundptr%d.%d", stmt.Index, labelCounter)
		currentindexboundIdent := fmt.Sprintf("%%currentindexbound%d.%d", stmt.Index, labelCounter+1)
		labelCounter += 2
		if _, err := fmt.Fprintf(w, "    %s = getelementptr %%arr_val, %%arr_val* %s,i32 0,i32 1,i32 %d\n", currentindexboundptrIdent, valptrIdent, i+1); err != nil {
			return 0, nil, "", err
		}
		if _, err := fmt.Fprintf(w, "    %s = load i32,i32* %s\n", currentindexboundIdent, currentindexboundptrIdent); err != nil {
			return 0, nil, "", err
		}

		outofboundscheckIdent := fmt.Sprintf("%%outofboundscheck%d.%d", stmt.Index, labelCounter)
		boundscheckOkLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+1)
		labelCounter += 2
		if _, err := fmt.Fprintf(w, "    %s = icmp ugt i32 %s,%s", outofboundscheckIdent, currentindexIdent, currentindexboundIdent); err != nil {
			return 0, nil, "", err
		}
		if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s\n  %s:\n", outofboundscheckIdent, boundsErrorLabel, boundscheckOkLabel, boundscheckOkLabel); err != nil {
			return 0, nil, "", err
		}

		if i == 0 {
			currentindextotalIdent = fmt.Sprintf("%%currentindextotal%d.%d", stmt.Index, labelCounter)
			labelCounter++
			currentindexmultiplierIdent = currentindexboundIdent
			if _, err := fmt.Fprintf(w, "    %s = sub i32 %s,1\n", currentindextotalIdent, currentindexIdent); err != nil {
				return 0, nil, "", err
			}
		} else {
			currentindexminusoneIdent := fmt.Sprintf("%%currentindexminusone%d.%d", stmt.Index, labelCounter)
			currentindexminusonetimesmultIdent := fmt.Sprintf("%%currentindexminusonetimesmult%d.%d", stmt.Index, labelCounter+1)
			newcurrentindextotalIdent := fmt.Sprintf("%%newcurrentindextotal%d.%d", stmt.Index, labelCounter+2)
			newcurrentindexmultiplierIdent := fmt.Sprintf("%%newcurrentindexmultiplier%d.%d", stmt.Index, labelCounter+3)
			labelCounter += 4
			if _, err := fmt.Fprintf(w, "    %s = sub i32 %s,1\n", currentindexminusoneIdent, currentindexIdent); err != nil {
				return 0, nil, "", err
			}
			if _, err := fmt.Fprintf(w, "    %s = mul i32 %s,%s\n", currentindexminusonetimesmultIdent, currentindexminusoneIdent, currentindexmultiplierIdent); err != nil {
				return 0, nil, "", err
			}
			if _, err := fmt.Fprintf(w, "    %s = add i32 %s,%s\n", newcurrentindextotalIdent, currentindexminusonetimesmultIdent, currentindextotalIdent); err != nil {
				return 0, nil, "", err
			}
			if _, err := fmt.Fprintf(w, "    %s = mul i32 %s,%s\n", newcurrentindexmultiplierIdent, currentindexboundIdent, currentindexmultiplierIdent); err != nil {
				return 0, nil, "", err
			}
			currentindextotalIdent = newcurrentindextotalIdent
			currentindexmultiplierIdent = newcurrentindexmultiplierIdent
		}
	}

	// find pointer to array element
	actualindexIdent := fmt.Sprintf("%%actualindex%d.%d", stmt.Index, labelCounter)
	arrayelementptrIdent := fmt.Sprintf("%%arrayelementptr%d.%d", stmt.Index, labelCounter+1)
	labelCounter += 2
	if _, err := fmt.Fprintf(w, "    %s = add i32 %s,%d\n", actualindexIdent, currentindextotalIdent, 1+len(arrayElement.Index)); err != nil {
		return 0, nil, "", err
	}
	if _, err := fmt.Fprintf(w, "    %s = getelementptr %%arr_val, %%arr_val* %s, i32 0, i32 1, i32 %s\n", arrayelementptrIdent, valptrIdent, actualindexIdent); err != nil {
		return 0, nil, "", err
	}
	return labelCounter, indexIdentSrcLabels, arrayelementptrIdent, nil
}

// returns newLabelCounter, ignoredIdent(i1), error
func codeGenCheckIgnored(w io.Writer, stmt *Statement, ignoredable Ignoredable, labelCounter int) (int, string, error) {
	vIdent := ""
	vType := ""
	switch v := ignoredable.(type) {
	case Array16:
		vIdent = fmt.Sprintf("@tail%d", v)
		vType = "%arr_vrbl"
	case Array32:
		vIdent = fmt.Sprintf("@hybrid%d", v)
		vType = "%arr_vrbl"
	case Var16:
		vIdent = fmt.Sprintf("@onespot%d", v)
		vType = "%vrbl"
	case Var32:
		vIdent = fmt.Sprintf("@twospot%d", v)
		vType = "%vrbl"
	case ArrayElement:
		return codeGenCheckIgnored(w, stmt, v.Array, labelCounter)
	default:
		panic("CheckIgnored")
	}

	ignoredptrIdent := fmt.Sprintf("%%ignoredptr%d.%d", stmt.Index, labelCounter)
	ignoredIdent := fmt.Sprintf("%%ignored%d.%d", stmt.Index, labelCounter+1)
	labelCounter += 2
	if _, err := fmt.Fprintf(w, "    %s = getelementptr %s, %s* %s, i32 0, i32 0\n", ignoredptrIdent, vType, vType, vIdent); err != nil {
		return 0, "", nil
	}
	if _, err := fmt.Fprintf(w, "    %s = load i1,i1* %s\n", ignoredIdent, ignoredptrIdent); err != nil {
		return 0, "", nil
	}

	return labelCounter, ignoredIdent, nil
}

func codeGenGets(w io.Writer, stmt *Statement, lhs LValue, rhsIdent string, doneLabel string, labelCounter int) (int, error) {
	if _, err := fmt.Fprintf(w, "    call void @check_error(%%val %s, i32 %d)\n", rhsIdent, stmt.Index+1); err != nil {
		return 0, err
	}

	rhsi32valueIdent := fmt.Sprintf("%%rhsi32value%d.%d", stmt.Index, labelCounter)
	labelCounter++
	if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1\n", rhsi32valueIdent, rhsIdent); err != nil {
		return 0, err
	}

	if lhs.Is16() {
		rhstagIdent := fmt.Sprintf("%%rhstag%d.%d", stmt.Index, labelCounter)
		rhstagisoneIdent := fmt.Sprintf("%%rhstagisone%d.%d", stmt.Index, labelCounter+1)
		dorangecheckLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+2)
		rangeokLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+3)
		labelCounter += 4
		if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,0\n", rhstagIdent, rhsIdent); err != nil {
			return 0, err
		}
		if _, err := fmt.Fprintf(w, "    %s = icmp eq i2 %s,1\n", rhstagisoneIdent, rhstagIdent); err != nil {
			return 0, err
		}
		if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s\n", rhstagisoneIdent, dorangecheckLabel, rangeokLabel); err != nil {
			return 0, err
		}

		rhs65535checkIdent := fmt.Sprintf("%%rhs65535check%d.%d", stmt.Index, labelCounter)
		failrangecheckLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+1)
		labelCounter += 2
		if _, err := fmt.Fprintf(w, "  %s:\n    %s = icmp uge i32 %s,65535 ; uge and not ugt: 4.4.1 says, '16-bit variables may be assigned 32-bit values only if the value is less than 65535.'\n", dorangecheckLabel, rhs65535checkIdent, rhsi32valueIdent); err != nil {
			return 0, err
		}
		if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s\n", rhs65535checkIdent, failrangecheckLabel, rangeokLabel); err != nil {
			return 0, err
		}
		if _, err := fmt.Fprintf(w, "  %s:\n    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 275,1),i32 %d)\n", failrangecheckLabel, stmt.Index+1); err != nil {
			return 0, err
		}
		if _, err := fmt.Fprintf(w, "    br label %%%s\n  %s:\n", rangeokLabel, rangeokLabel); err != nil {
			return 0, err
		}
	}

	switch lhs.(type) {
	case Var16, Var32:
		vIdent := ""
		if lhs.Is16() {
			vIdent = fmt.Sprintf("@onespot%d", lhs)
		} else {
			vIdent = fmt.Sprintf("@twospot%d", lhs)
		}
		valueptrptrIdent := fmt.Sprintf("%%valueptrptr%d.%d", stmt.Index, labelCounter)
		valueptrIdent := fmt.Sprintf("%%valueptr%d.%d", stmt.Index, labelCounter+1)
		valueptrisnullIdent := fmt.Sprintf("%%valueptrisnull%d.%d", stmt.Index, labelCounter+2)
		labelCounter += 3
		if _, err := fmt.Fprintf(w, "    %s = getelementptr %%vrbl, %%vrbl* %s,i32 0,i32 1\n", valueptrptrIdent, vIdent); err != nil {
			return 0, err
		}
		if _, err := fmt.Fprintf(w, "    %s = load %%vrbl_val*, %%vrbl_val** %s\n", valueptrIdent, valueptrptrIdent); err != nil {
			return 0, err
		}
		if _, err := fmt.Fprintf(w, "    %s = icmp eq %%vrbl_val* %s,null\n", valueptrisnullIdent, valueptrIdent); err != nil {
			return 0, err
		}
		valueptrisnullLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter)
		valueptrnotnullLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+1)
		labelCounter += 2
		if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s\n", valueptrisnullIdent, valueptrisnullLabel, valueptrnotnullLabel); err != nil {
			return 0, err
		}

		storevalueLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter)
		labelCounter++
		if _, err := fmt.Fprintf(w, "  %s:\n    br label %%%s\n", valueptrnotnullLabel, storevalueLabel); err != nil {
			return 0, err
		}

		// malloc new value
		mallocresultIdent := fmt.Sprintf("%%mallocresult%d.%d", stmt.Index, labelCounter)
		newvalueptrIdent := fmt.Sprintf("%%newvalueptr%d.%d", stmt.Index, labelCounter+1)
		labelCounter += 2
		if _, err := fmt.Fprintf(w, "  %s:\n    %s = call i8* @malloc(i32 ptrtoint(%%vrbl_val* getelementptr(%%vrbl_val,%%vrbl_val* null,i32 1) to i32))\n", valueptrisnullLabel, mallocresultIdent); err != nil {
			return 0, err
		}
		if _, err := fmt.Fprintf(w, "    call void @llvm.memset.p0i8.i32(i8* %s,i8 0,i32 ptrtoint(%%vrbl_val* getelementptr(%%vrbl_val,%%vrbl_val* null,i32 1) to i32),i32 0,i1 0)\n", mallocresultIdent); err != nil {
			return 0, err
		}
		if _, err := fmt.Fprintf(w, "    %s = bitcast i8* %s to %%vrbl_val*\n", newvalueptrIdent, mallocresultIdent); err != nil {
			return 0, err
		}
		if _, err := fmt.Fprintf(w, "    store %%vrbl_val* %s, %%vrbl_val** %s\n", newvalueptrIdent, valueptrptrIdent); err != nil {
			return 0, err
		}
		if _, err := fmt.Fprintf(w, "    br label %%%s\n", storevalueLabel); err != nil {
			return 0, err
		}

		// store value
		finalvalueptrIdent := fmt.Sprintf("%%finalvalueptr%d.%d", stmt.Index, labelCounter)
		finali32valueptrIdent := fmt.Sprintf("%%finali32valueptr%d.%d", stmt.Index, labelCounter+1)
		labelCounter += 2
		if _, err := fmt.Fprintf(w, "  %s:\n    %s = phi %%vrbl_val* [%s,%%%s],[%s,%%%s]\n", storevalueLabel, finalvalueptrIdent, valueptrIdent, valueptrnotnullLabel, newvalueptrIdent, valueptrisnullLabel); err != nil {
			return 0, err
		}
		if _, err := fmt.Fprintf(w, "    %s = getelementptr %%vrbl_val, %%vrbl_val* %s, i32 0, i32 1\n", finali32valueptrIdent, finalvalueptrIdent); err != nil {
			return 0, err
		}
		if _, err := fmt.Fprintf(w, "    store i32 %s,i32* %s\n    br label %%%s\n", rhsi32valueIdent, finali32valueptrIdent, doneLabel); err != nil {
			return 0, err
		}
		return labelCounter, nil

	case ArrayElement:
		indexErrorLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter)
		boundsErrorLabel := fmt.Sprintf("stmt%d.%d", stmt.Index, labelCounter+1)
		labelCounter += 2

		newLabelCounter, indexIdentSrcLabels, arrayElementPtrIdent, err := codeGenFindArrayElement(w, stmt, boundsErrorLabel, indexErrorLabel, lhs.(ArrayElement), labelCounter)
		if err != nil {
			return 0, err
		}
		labelCounter = newLabelCounter

		if _, err := fmt.Fprintf(w, "    store i32 %s,i32* %s\n", rhsi32valueIdent, arrayElementPtrIdent); err != nil {
			return 0, err
		}
		if _, err := fmt.Fprintf(w, "    br label %%%s\n", doneLabel); err != nil {
			return 0, err
		}

		if _, err := fmt.Fprintf(w, "  %s:\n    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 241,1),i32 %d)\n    br label %%%s\n", boundsErrorLabel, stmt.Index+1, doneLabel); err != nil {
			return 0, err
		}

		indexerrorIdent := fmt.Sprintf("%%indexerror%d.%d", stmt.Index, labelCounter)
		labelCounter++
		if _, err := fmt.Fprintf(w, "    %s:\n    %s = phi %%val ", indexErrorLabel, indexerrorIdent); err != nil {
			return 0, err
		}
		for i, indexIdentSrcLabel := range indexIdentSrcLabels {
			comma := ","
			if i == 0 {
				comma = ""
			}
			if _, err := fmt.Fprintf(w, "%s[%s,%%%s]", comma, indexIdentSrcLabel[0], indexIdentSrcLabel[1]); err != nil {
				return 0, err
			}
		}
		if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val %s,i32 %d)\n    br label %%%s\n", indexerrorIdent, stmt.Index+1, doneLabel); err != nil {
			return 0, err
		}

		return labelCounter, nil

	default:
		panic("Gets")
	}
}
