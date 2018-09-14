package intercal

import (
	"fmt"
	"io"
)

func CodeGen(statements []*Statement, llvmVersion int, w io.Writer) error {
	state := codeGenState{statements: statements, llvmVersion: llvmVersion}
	state.collectStmtInfo()
	state.collectVarInfo()
	if err := state.genListing(w); err != nil {
		return err
	}
	if err := state.genErrorMessages(w); err != nil {
		return err
	}
	if err := state.genGlobals(w); err != nil {
		return err
	}
	if err := state.genDebugInfo(w); err != nil {
		return err
	}
	if err := state.genMain(w); err != nil {
		return err
	}
	return nil
}

type codeGenState struct {
	statements  []*Statement
	llvmVersion int

	stmtInfo      []*codeGenStmt
	spots         map[Var16]*codeGenVar
	twospots      map[Var32]*codeGenVar
	tails         map[Array16]*codeGenVar
	hybrids       map[Array32]*codeGenVar
	mainDebugInfo string

	listingSize    int
	listingIndexes [][2]int // index, length

	errorMessagesSize    int
	errorMessagesIndexes [][3]int // error code, index, length

	stmt          *Statement
	debugLocation string
	labelCounter  int
}

type codeGenStmt struct {
	abstained     bool
	reinstated    bool
	debugLocation string
}

type codeGenVar struct {
	ident   string
	is16    bool
	varType string
	valType string

	assigned    bool
	accessed    bool
	dimensioned bool
	ignored     bool
	remembered  bool
	stashed     bool
	retrieved   bool

	dims []int
}

func (cgs *codeGenState) collectStmtInfo() {
	for _ = range cgs.statements {
		cgs.stmtInfo = append(cgs.stmtInfo, &codeGenStmt{})
	}
	for _, stmt := range cgs.statements {
		if stmt.Error != nil {
			continue
		}
		switch stmt.Type {
		case StatementAbstainLabel, StatementAbstainGerundList:
			for _, index := range stmt.Operands.([]int) {
				cgs.stmtInfo[index].abstained = true
			}
		case StatementReinstateLabel, StatementReinstateGerundList:
			for _, index := range stmt.Operands.([]int) {
				cgs.stmtInfo[index].reinstated = true
			}
		}
	}
}

func (cgs *codeGenState) collectVarInfo() {
	cgs.spots = make(map[Var16]*codeGenVar)
	cgs.twospots = make(map[Var32]*codeGenVar)
	cgs.tails = make(map[Array16]*codeGenVar)
	cgs.hybrids = make(map[Array32]*codeGenVar)
	for _, stmt := range cgs.statements {
		if stmt.Error != nil {
			continue
		}
		switch stmt.Type {
		case StatementUnrecognizable:

		case StatementCalculate:
			calculation := stmt.Operands.(Calculation)
			cgs.collectStmtVarInfo(calculation.LHS, stmt.Type)
			cgs.collectStmtVarInfo(calculation.RHS, StatementUnrecognizable)
		case StatementCalculateArrayDimension:
			dim := stmt.Operands.(Dimensioning)
			cgs.collectVarDimInfo(dim)
			for _, expr := range dim.RHS {
				cgs.collectStmtVarInfo(expr, StatementUnrecognizable)
			}
		case StatementNext:

		case StatementForget, StatementResume:
			cgs.collectStmtVarInfo(stmt.Operands, stmt.Type)
		case StatementStash, StatementRetrieve, StatementIgnore, StatementRemember:
			for _, arg := range stmt.Operands.([]Stashable) {
				cgs.collectStmtVarInfo(arg, stmt.Type)
			}
		case StatementAbstainLabel, StatementAbstainGerundList, StatementReinstateLabel, StatementReinstateGerundList:

		case StatementGiveUp:

		case StatementWriteIn:
			for _, arg := range stmt.Operands.([]WriteInable) {
				cgs.collectStmtVarInfo(arg, stmt.Type)
			}
		case StatementReadOut:
			for _, arg := range stmt.Operands.([]ReadOutable) {
				cgs.collectStmtVarInfo(arg, stmt.Type)
			}
		case StatementReadOutBit, StatementWriteIntoBit:

		case StatementLibrary:
			stmt.Operands.(LibraryFunction).CollectVarInfo(cgs)

		case StatementComeFromLabel, StatementComeFromGerundList, StatementNextFromLabel, StatementNextFromGerundList:

		default:
			panic("VarInfo")
		}
	}
}

func (cgs *codeGenState) collectStmtVarInfo(arg interface{}, stmtType StatementType) *codeGenVar {
	var varInfo *codeGenVar
	switch e := arg.(type) {
	case Var16:
		info, ok := cgs.spots[e]
		if !ok {
			info = &codeGenVar{
				ident:   fmt.Sprintf("@onespot%d", e),
				is16:    true,
				varType: "%vrbl",
				valType: "%vrbl_val",
			}
			cgs.spots[e] = info
		}
		varInfo = info
	case Var32:
		info, ok := cgs.twospots[e]
		if !ok {
			info = &codeGenVar{
				ident:   fmt.Sprintf("@twospot%d", e),
				is16:    false,
				varType: "%vrbl",
				valType: "%vrbl_val",
			}
			cgs.twospots[e] = info
		}
		varInfo = info
	case Array16:
		info, ok := cgs.tails[e]
		if !ok {
			info = &codeGenVar{
				ident:   fmt.Sprintf("@tail%d", e),
				is16:    true,
				varType: "%arr_vrbl",
				valType: "%arr_val",
			}
			cgs.tails[e] = info
		}
		varInfo = info
	case Array32:
		info, ok := cgs.hybrids[e]
		if !ok {
			info = &codeGenVar{
				ident:   fmt.Sprintf("@hybrid%d", e),
				is16:    false,
				varType: "%arr_vrbl",
				valType: "%arr_val",
			}
			cgs.hybrids[e] = info
		}
		varInfo = info
	case ArrayElement:
		cgs.collectStmtVarInfo(e.Array, stmtType)
		for _, expr := range e.Index {
			cgs.collectStmtVarInfo(expr, StatementUnrecognizable)
		}
	case ExprConst:
	case ExprMingle:
		cgs.collectStmtVarInfo(e[0], StatementUnrecognizable)
		cgs.collectStmtVarInfo(e[1], StatementUnrecognizable)
	case ExprSelect:
		cgs.collectStmtVarInfo(e[0], StatementUnrecognizable)
		cgs.collectStmtVarInfo(e[1], StatementUnrecognizable)
	case ExprAnd:
		cgs.collectStmtVarInfo(e[0], StatementUnrecognizable)
	case ExprOr:
		cgs.collectStmtVarInfo(e[0], StatementUnrecognizable)
	case ExprXor:
		cgs.collectStmtVarInfo(e[0], StatementUnrecognizable)
	default:
		panic("StmtVarInfo")
	}
	if varInfo != nil {
		switch stmtType {
		case StatementUnrecognizable:
			varInfo.accessed = true
		case StatementCalculate:
			varInfo.assigned = true
		case StatementCalculateArrayDimension:
			varInfo.dimensioned = true
		case StatementForget, StatementResume:
			varInfo.accessed = true
		case StatementStash:
			varInfo.stashed = true
		case StatementRetrieve:
			varInfo.retrieved = true
		case StatementIgnore:
			varInfo.ignored = true
		case StatementRemember:
			varInfo.remembered = true
		case StatementWriteIn:
			varInfo.assigned = true
		case StatementReadOut:
			varInfo.accessed = true
		case StatementLibrary:
		case StatementComeFromLabel, StatementComeFromGerundList, StatementNextFromLabel, StatementNextFromGerundList:
		default:
			panic("StmtVarInfo")
		}
	}
	return varInfo
}

func (cgs *codeGenState) collectVarDimInfo(dim Dimensioning) {
	cgs.collectStmtVarInfo(dim.LHS, StatementCalculateArrayDimension)
	varInfo := cgs.varInfo(dim.LHS)
	for _, d := range varInfo.dims {
		if d == len(dim.RHS) {
			return
		}
	}
	varInfo.dims = append(varInfo.dims, len(dim.RHS))
}

func (cgs *codeGenState) varInfo(vrbl Ignoredable) *codeGenVar {
	switch v := vrbl.(type) {
	case Var16:
		return cgs.spots[v]
	case Var32:
		return cgs.twospots[v]
	case Array16:
		return cgs.tails[v]
	case Array32:
		return cgs.hybrids[v]
	case ArrayElement:
		return cgs.varInfo(v.Array)
	default:
		panic("VarInfo")
	}
}

func (cgs *codeGenState) genListing(w io.Writer) error {
	statementNumberIndexes := [][2]int{}
	for i, stmt := range cgs.statements {
		stmtNumber := fmt.Sprintf("%04d", i+1)
		stmtNumberIndex := cgs.listingSize + 1
		cgs.listingSize += 2 + len(stmtNumber)
		startIndex := cgs.listingSize
		cgs.listingSize += len(stmt.String()) + 1
		cgs.listingIndexes = append(cgs.listingIndexes, [2]int{startIndex, len(stmt.String())})
		statementNumberIndexes = append(statementNumberIndexes, [2]int{stmtNumberIndex, len(stmtNumber)})
	}
	statementNumberIndexes = append(statementNumberIndexes, [2]int{cgs.listingSize, 4}) // "nnnn" for error 633 or abstained
	cgs.listingSize += 4

	if _, err := fmt.Fprintf(w, "@program_listing = private constant [%d x i8] [", cgs.listingSize); err != nil {
		return err
	}
	for i, stmt := range cgs.statements {
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
		if _, err := fmt.Fprintf(w, "%s{i8*,i32}{i8* getelementptr([%d x i8], [%d x i8]* @program_listing, i32 0, i32 %d),i32 %d}", comma, cgs.listingSize, cgs.listingSize, index[0], index[1]); err != nil {
			return err
		}
	}
	if _, err := fmt.Fprintf(w, "]\n"); err != nil {
		return err
	}
	return nil
}

func (cgs *codeGenState) genErrorMessages(w io.Writer) error {
	// error messages
	errorMessagesSize := 0
	errorMessageIndexes := [][3]int{} // error code, index, length
	for _, e := range ErrorList {
		msg := e.Message()
		if msg == "" {
			errorMessageIndexes = append(errorMessageIndexes, [3]int{e.Code(), 0, 0})
		} else {
			m := fmt.Sprintf("ICL%03dI %s", e.MessageCode(), msg)
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
				m := fmt.Sprintf("ICL%03dI %s", e.MessageCode(), msg)
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
	if _, err := fmt.Fprintf(w, "define void @fatal_error(%%val %%err, i32 %%next_stmt_index) noreturn {\n  entry:\n    br label %%find_stmt_index\n  find_stmt_index:\n    %%stmt_index = phi i32 [%%next_stmt_index,%%entry],[%%stmt_index1,%%find_stmt_index]\n    %%stmt_index1 = add i32 %%stmt_index, 1\n    %%abstained_ptr = getelementptr [%d x i1], [%d x i1]* @abstain_flags, i32 0, i32 %%stmt_index\n    %%abstained = load i1, i1* %%abstained_ptr\n    br i1 %%abstained, label %%find_stmt_index, label %%check_err_tag\n", len(cgs.statements)+1, len(cgs.statements)+1); err != nil {
		return err
	}
	if _, err := fmt.Fprintf(w, "  check_err_tag:\n    %%stmt_number_info_ptr = getelementptr [%d x {i8*,i32}], [%d x {i8*,i32}]* @statement_numbers, i32 0, i32 %%stmt_index\n    %%stmt_number_info = load {i8*,i32}, {i8*,i32}* %%stmt_number_info_ptr\n    %%stmt_number = extractvalue {i8*,i32} %%stmt_number_info, 0\n    %%stmt_number_len = extractvalue {i8*,i32} %%stmt_number_info, 1\n    %%tag = extractvalue %%val %%err, 0\n    %%code = extractvalue %%val %%err, 1\n    %%tag_is2 = icmp eq i2 %%tag, 2\n    br i1 %%tag_is2, label %%err_code_index0, label %%err_nomsg\n  err_nomsg:\n    call void @finish_fatal_error(i8* null,i32 0,i8* %%stmt_number,i32 %%stmt_number_len) noreturn\n    ret void\n", len(cgs.statements)+1, len(cgs.statements)+1); err != nil {
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

	// void @fatal_error_on_the_way_to(i32 %err_code, i8* %stmt_ptr) noreturn
	if _, err := fmt.Fprintf(w, "define void @fatal_error_on_the_way_to(i32 %%err_code, i8* %%stmt_ptr) {\n"); err != nil {
		return err
	}
	for i := range cgs.statements {
		if _, err := fmt.Fprintf(w, "    %%check_stmt%d = icmp eq i8* %%stmt_ptr,blockaddress(@main,%%stmt%d)\n", i, i); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    br i1 %%check_stmt%d,label %%is_stmt%d,label %%not_stmt%d\n", i, i, i); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "  is_stmt%d:\n    %%err%d = insertvalue %%val insertvalue(%%val zeroinitializer,i2 2,0),i32 %%err_code,1\n", i, i); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val %%err%d,i32 %d)\n", i, i); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    ret void\n"); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "  not_stmt%d:\n", i); err != nil {
			return err
		}
	}
	if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 778,1),i32 %d)\n", len(cgs.statements)); err != nil {
		return err
	}
	if _, err := fmt.Fprintf(w, "    ret void\n}\n"); err != nil {
		return err
	}

	return nil
}

func (cgs *codeGenState) genGlobals(w io.Writer) error {
	// abstain flag global array
	if _, err := fmt.Fprintf(w, "@abstain_flags = global [%d x i1] [", len(cgs.statements)+1); err != nil {
		return err
	}
	for _, stmt := range cgs.statements {
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

	// future optimization: omit never assigned, never dimensioned
	for _, info := range cgs.spots {
		if _, err := fmt.Fprintf(w, "%s = global %s {i1 0,%s* %s_val}\n", info.ident, info.varType, info.valType, info.ident); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "%s_val = global %s zeroinitializer\n", info.ident, info.valType); err != nil {
			return err
		}
	}
	for _, info := range cgs.twospots {
		if _, err := fmt.Fprintf(w, "%s = global %s {i1 0,%s* %s_val}\n", info.ident, info.varType, info.valType, info.ident); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "%s_val = global %s zeroinitializer\n", info.ident, info.valType); err != nil {
			return err
		}
	}
	for _, info := range cgs.tails {
		if _, err := fmt.Fprintf(w, "%s = global %s zeroinitializer\n", info.ident, info.varType); err != nil {
			return err
		}
	}
	for _, info := range cgs.hybrids {
		if _, err := fmt.Fprintf(w, "%s = global %s zeroinitializer\n", info.ident, info.varType); err != nil {
			return err
		}
	}

	return nil
}

func (cgs *codeGenState) genDebugInfo(w io.Writer) error {
	if cgs.llvmVersion < 5 {
		return nil
	}

	hasLocations := false
	for _, stmt := range cgs.statements {
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
		return nil
	}

	if _, err := fmt.Fprintf(w, "!llvm.module.flags = !{!0, !1}\n"); err != nil {
		return err
	}
	if _, err := fmt.Fprintf(w, "!0 = !{i32 2, !\"Dwarf Version\", i32 4}\n"); err != nil {
		return err
	}
	if _, err := fmt.Fprintf(w, "!1 = !{i32 2, !\"Debug Info Version\", i32 3}\n"); err != nil {
		return err
	}

	metainfoCounter := 2
	files := make(map[string]int)
	compileUnits := []int{}
	subprogramMetainfo := 0
	for _, stmt := range cgs.statements {
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
		fileMetainfo := metainfoCounter
		metainfoCounter++
		if _, err := fmt.Fprintf(w, "!%d = !DIFile(filename: \"%s\", directory: \"%s\")\n", fileMetainfo, stmtToken.Location.Filename, stmtToken.Location.Dir); err != nil {
			return err
		}
		if subprogramMetainfo == 0 {
			compileUnitMetainfo := metainfoCounter
			compileUnits = append(compileUnits, compileUnitMetainfo)
			if _, err := fmt.Fprintf(w, "!%d = distinct !DICompileUnit(language: DW_LANG_C89, file: !%d, emissionKind: FullDebug)\n", compileUnitMetainfo, fileMetainfo); err != nil {
				return err
			}
			metainfoCounter++
			subprogramMetainfo = metainfoCounter
			typeMetainfo := metainfoCounter + 1
			if _, err := fmt.Fprintf(w, "!%d = distinct !DISubprogram(name: \"PROGRAM\", linkageName: \"main\", file: !%d, unit: !%d, type: !%d)\n", subprogramMetainfo, fileMetainfo, compileUnitMetainfo, typeMetainfo); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "!%d = !DISubroutineType(types: !{})\n", typeMetainfo); err != nil {
				return err
			}
			metainfoCounter += 2
			files[stmtToken.Location.Filename] = subprogramMetainfo
			cgs.mainDebugInfo = fmt.Sprintf(" !dbg !%d", subprogramMetainfo)
		} else {
			lexicalBlockFileMetainfo := metainfoCounter
			if _, err := fmt.Fprintf(w, "!%d = !DILexicalBlockFile(scope: !%d, file: !%d, discriminator: 0)\n", lexicalBlockFileMetainfo, subprogramMetainfo, fileMetainfo); err != nil {
				return err
			}
			metainfoCounter++
			files[stmtToken.Location.Filename] = lexicalBlockFileMetainfo
		}
	}

	if _, err := fmt.Fprintf(w, "!llvm.dbg.cu = !{"); err != nil {
		return err
	}
	for i, cu := range compileUnits {
		comma := ","
		if i == 0 {
			comma = ""
		}
		if _, err := fmt.Fprintf(w, "%s!%d", comma, cu); err != nil {
			return err
		}
	}
	if _, err := fmt.Fprintf(w, "}\n"); err != nil {
		return err
	}

	for _, stmt := range cgs.statements {
		var stmtToken *Token
		if stmt.Label == 0 && len(stmt.Tokens) > 0 {
			stmtToken = stmt.Tokens[0]
		} else if stmt.Label > 0 && len(stmt.Tokens) > 3 {
			stmtToken = stmt.Tokens[3]
		}
		if stmtToken != nil && stmtToken.Location.Filename != "" {
			if _, err := fmt.Fprintf(w, "!%d = !DILocation(line: %d, column: %d, scope: !%d)\n", metainfoCounter, stmtToken.Location.Line, stmtToken.Location.Column, files[stmtToken.Location.Filename]); err != nil {
				return err
			}
			cgs.stmtInfo[stmt.Index].debugLocation = fmt.Sprintf(", !dbg !%d", metainfoCounter)
			metainfoCounter++
		}
	}
	return nil
}

func (cgs *codeGenState) genMain(w io.Writer) error {
	// void @main()
	if _, err := fmt.Fprintf(w, "define void @main()%s {\n    call i32 @write(i32 2, i8* getelementptr([%d x i8],[%d x i8]* @program_listing,i32 0,i32 0),i32 %d)\n    %%random_seed = call i32 @time(i8* null)\n    call void @srandom(i32 %%random_seed)\n    br label %%stmt0\n", cgs.mainDebugInfo, cgs.listingSize, cgs.listingSize, cgs.listingSize-4); err != nil {
		return err
	}

	for i := range cgs.statements {
		cgs.stmt = cgs.statements[i]
		cgs.debugLocation = cgs.stmtInfo[i].debugLocation
		cgs.labelCounter = 0
		if err := cgs.codeGenStmt(w); err != nil {
			return err
		}
	}

	if _, err := fmt.Fprintf(w, "  stmt%d:\n    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 633,1),i32 %d) noreturn\n    ret void\n}\n", len(cgs.statements), len(cgs.statements)); err != nil {
		return err
	}

	return nil
}

func (cgs *codeGenState) ident(name string) string {
	cgs.labelCounter++
	return fmt.Sprintf("%%%s%d.%d", name, cgs.stmt.Index, cgs.labelCounter)
}

func (cgs *codeGenState) label(name string) string {
	cgs.labelCounter++
	return fmt.Sprintf("%s%d.%d", name, cgs.stmt.Index, cgs.labelCounter)
}

func (cgs *codeGenState) nextIndex() int {
	switch cgs.stmt.Type {
	case StatementNext:
		if cgs.stmt.Operands != nil {
			return cgs.stmt.Operands.(int)
		}
	case StatementResume:
		return len(cgs.statements)
	}
	return cgs.stmt.Index + 1
}

func (cgs *codeGenState) codeGenStmt(w io.Writer) error {
	if _, err := fmt.Fprintf(w, "  stmt%d:\n", cgs.stmt.Index); err != nil {
		return err
	}
	if cgs.stmt.Label != 0 && len(cgs.stmt.Tokens) > 3 {
		if _, err := fmt.Fprintf(w, "    ; %s:%d:%d\n", cgs.stmt.Tokens[3].Location.Filename, cgs.stmt.Tokens[3].Location.Line, cgs.stmt.Tokens[3].Location.Column); err != nil {
			return err
		}
	} else if len(cgs.stmt.Tokens) > 0 {
		if _, err := fmt.Fprintf(w, "    ; %s:%d:%d\n", cgs.stmt.Tokens[0].Location.Filename, cgs.stmt.Tokens[0].Location.Line, cgs.stmt.Tokens[0].Location.Column); err != nil {
			return err
		}
	}

	// Err774 check
	err774Ident := ""
	{
		doErr774 := cgs.stmt.Please && cgs.stmt.Type != StatementUnrecognizable
		for i := cgs.stmt.Index; i < len(cgs.statements) && i <= cgs.stmt.Index+10; i++ {
			doErr774 = doErr774 && !cgs.statements[i].Thank
		}
		if doErr774 && cgs.stmt.Type != StatementResume {
			ident1 := cgs.ident("stmt")
			label1 := cgs.label("stmt")
			label2 := cgs.label("stmt")
			if _, err := fmt.Fprintf(w, "    %s = call i1 @random_check(i32 5)%s\n", ident1, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    br i1 %s, label %%%s, label %%%s%s\n", ident1, label1, label2, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "  %s:\n    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 774,1),i32 %d) noreturn%s\n", label1, cgs.nextIndex(), cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    ret void%s\n  %s:\n", cgs.debugLocation, label2); err != nil {
				return err
			}
		} else if doErr774 && cgs.stmt.Type == StatementResume {
			err774Ident = cgs.ident("stmt")
			if _, err := fmt.Fprintf(w, "    %s = call i1 @random_check(i32 14)%s\n", err774Ident, cgs.debugLocation); err != nil {
				return err
			}
		}
	}

	// skip statements that will never execute
	if cgs.stmt.Chance == 0 || (cgs.stmt.Not && !cgs.stmtInfo[cgs.stmt.Index].reinstated) {
		if err := cgs.genGotoNext(w, cgs.stmt.Index+1, ""); err != nil {
			return err
		}
		return nil
	}

	// ABSTAIN check
	if cgs.stmt.Not || cgs.stmtInfo[cgs.stmt.Index].abstained {
		abstainPtrIdent := cgs.ident("abstain_ptr")
		if _, err := fmt.Fprintf(w, "    %s = getelementptr [%d x i1], [%d x i1]* @abstain_flags,i32 0,i32 %d%s\n", abstainPtrIdent, len(cgs.statements)+1, len(cgs.statements)+1, cgs.stmt.Index, cgs.debugLocation); err != nil {
			return err
		}
		abstainIdent := cgs.ident("abstain")
		if _, err := fmt.Fprintf(w, "    %s = load i1, i1* %s%s\n", abstainIdent, abstainPtrIdent, cgs.debugLocation); err != nil {
			return err
		}
		abstainedLabel := cgs.label("stmt")
		notAbstainedLabel := cgs.label("stmt")
		if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", abstainIdent, abstainedLabel, notAbstainedLabel, cgs.debugLocation, abstainedLabel); err != nil {
			return err
		}
		if err := cgs.genGotoNext(w, cgs.stmt.Index+1, ""); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "  %s:\n", notAbstainedLabel); err != nil {
			return err
		}
	}

	if cgs.stmt.Error != nil && cgs.stmt.Error.Code() != 0 {
		if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 %d,1),i32 %d) noreturn%s\n    ret void%s\n", cgs.stmt.Error.Code(), cgs.stmt.Index+1, cgs.debugLocation, cgs.debugLocation); err != nil {
			return err
		}
		return nil
	}

	if cgs.stmt.Type == StatementUnrecognizable || (cgs.stmt.Error != nil && cgs.stmt.Error.Code() == 0) {
		if _, err := fmt.Fprintf(w, "    ;SYNTAX ERROR\n    call i32 @write(i32 2, i8* getelementptr([8 x i8], [8 x i8]* @error_message_000_prefix,i32 0,i32 0),i32 8)%s\n    call i32 @write(i32 2, i8* getelementptr([%d x i8], [%d x i8]* @program_listing,i32 0,i32 %d),i32 %d)%s\n", cgs.debugLocation, cgs.listingSize, cgs.listingSize, cgs.listingIndexes[cgs.stmt.Index][0], cgs.listingIndexes[cgs.stmt.Index][1], cgs.debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 3,0),i32 0,1),i32 %d) noreturn%s\n    ret void%s\n", cgs.stmt.Index+1, cgs.debugLocation, cgs.debugLocation); err != nil {
			return err
		}
		return nil
	}

	// probability check
	countIdent := ""
	if cgs.stmt.Chance%100 == 0 {
		countIdent = fmt.Sprintf("%d", cgs.stmt.Chance/100)
	} else {
		randomCheckIdent := cgs.ident("random_check")
		if _, err := fmt.Fprintf(w, "    %s = call i1 @random_check(i32 %d)%s\n", randomCheckIdent, 10*(cgs.stmt.Chance%100), cgs.debugLocation); err != nil {
			return err
		}
		if cgs.stmt.Chance > 100 {
			countIdent := cgs.ident("run_stmt_count")
			if _, err := fmt.Fprintf(w, "    %s = select i1 %s,i32 %d,i32 %d%s\n", countIdent, randomCheckIdent, cgs.stmt.Chance/100+1, cgs.stmt.Chance, cgs.debugLocation); err != nil {
				return err
			}
		} else {
			doStmtLabel := cgs.label("do_stmt")
			skipStmtLabel := cgs.label("skip_stmt")
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", randomCheckIdent, doStmtLabel, skipStmtLabel, cgs.debugLocation, skipStmtLabel); err != nil {
				return err
			}
			if err := cgs.genGotoNext(w, cgs.stmt.Index+1, ""); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "  %s:\n", doStmtLabel); err != nil {
				return err
			}
		}
	}

	switch cgs.stmt.Type {
	case StatementCalculate:
		if _, err := fmt.Fprintf(w, "    ;CALCULATE\n"); err != nil {
			return err
		}
		doneLabel := cgs.label("calculate_done")
		calc := cgs.stmt.Operands.(Calculation)
		if ignoredIdent, err := cgs.genCheckIgnored(w, calc.LHS); err != nil {
			return err
		} else {
			notignoredLabel := cgs.label("not_ignored")
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", ignoredIdent, doneLabel, notignoredLabel, cgs.debugLocation, notignoredLabel); err != nil {
				return err
			}
		}
		enterLoopLabel := cgs.label("enter_loop")
		loopLabel := cgs.label("loop")
		loopEndLabel := cgs.label("loop_end")
		if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n    br label %%%s%s\n  %s:\n", enterLoopLabel, cgs.debugLocation, enterLoopLabel, loopLabel, cgs.debugLocation, loopLabel); err != nil {
			return err
		}
		if cgs.stmt.Chance > 100 {
			loopCounterIdent := cgs.ident("loop_counter")
			nextLoopCounterIdent := cgs.ident("next_loop_counter")
			if _, err := fmt.Fprintf(w, "    %s = phi i32 [0,%%%s],[%s,%%%s]%s\n", loopCounterIdent, enterLoopLabel, nextLoopCounterIdent, loopEndLabel, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    %s = add i32 %s,1%s\n", nextLoopCounterIdent, loopCounterIdent, cgs.debugLocation); err != nil {
				return err
			}
			loopCounterCheckIdent := cgs.ident("loop_counter_check")
			if _, err := fmt.Fprintf(w, "    %s = icmp uge i32 %s,%s%s\n", loopCounterCheckIdent, loopCounterIdent, countIdent, cgs.debugLocation); err != nil {
				return err
			}
			loopBodyLabel := cgs.label("loop_body")
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", loopCounterCheckIdent, doneLabel, loopBodyLabel, cgs.debugLocation, loopBodyLabel); err != nil {
			}
		}
		if rhsIdent, err := cgs.genExpr(w, calc.RHS); err != nil {
			return err
		} else if err := cgs.genGets(w, calc.LHS, rhsIdent, loopEndLabel); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "  %s:\n", loopEndLabel); err != nil {
			return err
		}
		if cgs.stmt.Chance > 100 {
			if _, err := fmt.Fprintf(w, "    br label %%%s%s", loopLabel, cgs.debugLocation); err != nil {
				return err
			}
		} else {
			if _, err := fmt.Fprintf(w, "    br label %%%s%s", doneLabel, cgs.debugLocation); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(w, "  %s:\n", doneLabel); err != nil {
			return err
		}
		if err := cgs.genGotoNext(w, cgs.nextIndex(), ""); err != nil {
			return err
		}

	case StatementCalculateArrayDimension:
		if _, err := fmt.Fprintf(w, "    ;CALCULATE\n"); err != nil {
			return err
		}
		dim := cgs.stmt.Operands.(Dimensioning)
		doneLabel := cgs.label("calculate_done")
		if ignoredIdent, err := cgs.genCheckIgnored(w, dim.LHS); err != nil {
			return err
		} else {
			notignoredLabel := cgs.label("stmt")
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", ignoredIdent, doneLabel, notignoredLabel, cgs.debugLocation, notignoredLabel); err != nil {
				return err
			}
		}

		totaldimIdent := ""
		dimIdents := []string{}
		for i, dimExpr := range dim.RHS {
			dimexprIdent, err := cgs.genExpr(w, dimExpr)
			if err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    call void @check_error(%%val %s,i32 %d)%s\n", dimexprIdent, cgs.nextIndex(), cgs.debugLocation); err != nil {
				return err
			}

			dimIdent := cgs.ident("dim")
			dimiszeroIdent := cgs.ident("dim_is_zero")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", dimIdent, dimexprIdent, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    %s = icmp eq i32 %s,0%s\n", dimiszeroIdent, dimIdent, cgs.debugLocation); err != nil {
				return err
			}

			dimiszeroLabel := cgs.label("stmt")
			dimisokLabel := cgs.label("stmt")
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n", dimiszeroIdent, dimiszeroLabel, dimisokLabel, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "  %s:\n    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 240,1),i32 %d) noreturn%s\n    ret void%s\n", dimiszeroLabel, cgs.nextIndex(), cgs.debugLocation, cgs.debugLocation); err != nil {
				return err
			}

			if _, err := fmt.Fprintf(w, "  %s:\n", dimisokLabel); err != nil {
				return err
			}

			dimIdents = append(dimIdents, dimIdent)
			if i == 0 {
				totaldimIdent = dimIdent
			} else {
				newtotaldimIdent := cgs.ident("total_dim")
				if _, err := fmt.Fprintf(w, "    %s = mul i32 %s,%s%s\n", newtotaldimIdent, totaldimIdent, dimIdent, cgs.debugLocation); err != nil {
					return err
				}
				totaldimIdent = newtotaldimIdent
			}
		}

		i32arrsizeIdent := cgs.ident("i32_array_size")
		mallocsizeaddrIdent := cgs.ident("malloc_size_addr")
		mallocsizeIdent := cgs.ident("malloc_size")
		if _, err := fmt.Fprintf(w, "    %s = add i32 %d,%s%s\n", i32arrsizeIdent, len(dim.RHS), totaldimIdent, cgs.debugLocation); err != nil {
			return nil
		}
		if _, err := fmt.Fprintf(w, "    %s = getelementptr %%arr_val,%%arr_val* null,i32 0,i32 3,i32 %s%s\n", mallocsizeaddrIdent, i32arrsizeIdent, cgs.debugLocation); err != nil {
			return nil
		}
		if _, err := fmt.Fprintf(w, "    %s = ptrtoint i32* %s to i32%s\n", mallocsizeIdent, mallocsizeaddrIdent, cgs.debugLocation); err != nil {
			return nil
		}

		mallocresultIdent := cgs.ident("malloc_result")
		newarrvalIdent := cgs.ident("new_arr_val")
		newarrvaldimptrIdent := cgs.ident("new_arr_val_dim")
		if _, err := fmt.Fprintf(w, "    %s = call i8* @malloc(i32 %s)%s\n", mallocresultIdent, mallocsizeIdent, cgs.debugLocation); err != nil {
			return err
		}
		if err := cgs.genMemset(w, mallocresultIdent, "0", mallocsizeIdent); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    %s = bitcast i8* %s to %%arr_val*%s\n", newarrvalIdent, mallocresultIdent, cgs.debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    %s = getelementptr %%arr_val,%%arr_val* %s,i32 0,i32 2%s\n    store i32 %d,i32* %s%s\n", newarrvaldimptrIdent, newarrvalIdent, cgs.debugLocation, len(dim.RHS), newarrvaldimptrIdent, cgs.debugLocation); err != nil {
			return err
		}
		for i, dimIdent := range dimIdents {
			newdimptrIdent := cgs.ident("new_dim_ptr")
			if _, err := fmt.Fprintf(w, "    %s = getelementptr %%arr_val,%%arr_val* %s,i32 0,i32 3,i32 %d%s\n", newdimptrIdent, newarrvalIdent, i, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    store i32 %s,i32* %s%s\n", dimIdent, newdimptrIdent, cgs.debugLocation); err != nil {
				return err
			}
		}

		arrvalptrIdent := cgs.ident("arr_val_ptr")
		varInfo := cgs.varInfo(dim.LHS)
		if _, err := fmt.Fprintf(w, "    %s = getelementptr %s,%s* %s,i32 0,i32 1%s\n", arrvalptrIdent, varInfo.varType, varInfo.varType, varInfo.ident, cgs.debugLocation); err != nil {
			return err
		}

		if varInfo.retrieved {
			oldstashIdent := cgs.ident("old_stash")
			if _, err := fmt.Fprintf(w, "    %s = call %%arr_val* @lazy_stash_arr_cow(%%arr_vrbl* %s,i1 0)%s\n", oldstashIdent, varInfo.ident, cgs.debugLocation); err != nil {
				return err
			}
			newstashptrIdent := cgs.ident("new_stash_ptr")
			if _, err := fmt.Fprintf(w, "    %s = getelementptr %%arr_val,%%arr_val* %s,i32 0,i32 0%s\n", newstashptrIdent, newarrvalIdent, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    store %%arr_val* %s,%%arr_val** %s%s\n", oldstashIdent, newstashptrIdent, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    store %%arr_val* %s,%%arr_val** %s%s\n", newarrvalIdent, arrvalptrIdent, cgs.debugLocation); err != nil {
				return err
			}
		} else {
			oldarrvalIdent := cgs.ident("old_arr_val")
			oldarrvalisnullIdent := cgs.ident("old_arr_val_is_null")
			freeoldarrvalLabel := cgs.label("stmt")
			storenewarrvalLabel := cgs.label("stmt")
			if _, err := fmt.Fprintf(w, "    %s = load %%arr_val*,%%arr_val** %s%s\n", oldarrvalIdent, arrvalptrIdent, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    %s = icmp eq %%arr_val* %s,null%s\n", oldarrvalisnullIdent, oldarrvalIdent, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n", oldarrvalisnullIdent, storenewarrvalLabel, freeoldarrvalLabel, cgs.debugLocation); err != nil {
				return err
			}

			oldstashptrIdent := cgs.ident("old_stash_ptr")
			oldstashIdent := cgs.ident("old_stash")
			newstashptrIdent := cgs.ident("new_stash_ptr")
			oldarrvali8ptrIdent := cgs.ident("old_arr_val_i8_ptr")
			if _, err := fmt.Fprintf(w, "  %s:\n    %s = getelementptr %%arr_val,%%arr_val* %s,i32 0,i32 0%s\n", freeoldarrvalLabel, oldstashptrIdent, oldarrvalIdent, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    %s = load %%arr_val*,%%arr_val** %s%s\n", oldstashIdent, oldstashptrIdent, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    %s = getelementptr %%arr_val,%%arr_val* %s,i32 0,i32 0%s\n", newstashptrIdent, newarrvalIdent, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    store %%arr_val* %s,%%arr_val** %s%s\n", oldstashIdent, newstashptrIdent, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    %s = bitcast %%arr_val* %s to i8*%s\n", oldarrvali8ptrIdent, oldarrvalIdent, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    call void @free(i8* %s)%s\n", oldarrvali8ptrIdent, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    br label %%%s%s\n", storenewarrvalLabel, cgs.debugLocation); err != nil {
				return err
			}

			if _, err := fmt.Fprintf(w, "  %s:\n    store %%arr_val* %s,%%arr_val** %s%s\n", storenewarrvalLabel, newarrvalIdent, arrvalptrIdent, cgs.debugLocation); err != nil {
				return err
			}
		}

		if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n", doneLabel, cgs.debugLocation, doneLabel); err != nil {
			return err
		}
		if err := cgs.genGotoNext(w, cgs.nextIndex(), ""); err != nil {
			return err
		}

	case StatementNext:
		if _, err := fmt.Fprintf(w, "    ;NEXT\n"); err != nil {
			return err
		}
		if err := cgs.genGotoNext(w, cgs.nextIndex(), countIdent); err != nil {
			return err
		}

	case StatementForget:
		if _, err := fmt.Fprintf(w, "    ;FORGET\n"); err != nil {
			return err
		}
		argValIdent := ""
		if exprIdent, err := cgs.genExpr(w, cgs.stmt.Operands.(Expr)); err != nil {
			return err
		} else {
			argValIdent = exprIdent
		}
		if _, err := fmt.Fprintf(w, "    call void @check_error(%%val %s, i32 %d)%s\n", argValIdent, cgs.nextIndex(), cgs.debugLocation); err != nil {
			return err
		}
		argIdent := cgs.ident("arg")
		if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s, 1%s\n", argIdent, argValIdent, cgs.debugLocation); err != nil {
			return err
		}
		argTimesCountIdent := cgs.ident("arg_times_count")
		if _, err := fmt.Fprintf(w, "    %s = mul i32 %s,%s%s\n", argTimesCountIdent, argIdent, countIdent, cgs.debugLocation); err != nil {
			return err
		}
		stackptrIdent := cgs.ident("stackptr")
		if _, err := fmt.Fprintf(w, "    %s = load i32, i32* @stackptr%s\n", stackptrIdent, cgs.debugLocation); err != nil {
			return err
		}

		stackptrCheckIdent := cgs.ident("stackptr_check")
		if _, err := fmt.Fprintf(w, "    %s = icmp uge i32 %s, %s%s\n", stackptrCheckIdent, argTimesCountIdent, stackptrIdent, cgs.debugLocation); err != nil {
			return err
		}

		stackptrUnderflowLabel := cgs.label("stack_underflow")
		stackptrOkLabel := cgs.label("stack_ok")
		nextStackptrIdent := cgs.ident("next_stackptr")
		doneLabel := cgs.label("forget_done")
		if _, err := fmt.Fprintf(w, "    br i1 %s, label %%%s, label %%%s%s\n", stackptrCheckIdent, stackptrUnderflowLabel, stackptrOkLabel, cgs.debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "  %s:\n    store i32 0,i32* @stackptr%s\n    br label %%%s%s\n", stackptrUnderflowLabel, cgs.debugLocation, doneLabel, cgs.debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "  %s:\n    %s = sub i32 %s, %s%s\n    store i32 %s,i32* @stackptr%s\n    br label %%%s%s\n", stackptrOkLabel, nextStackptrIdent, stackptrIdent, argTimesCountIdent, cgs.debugLocation, nextStackptrIdent, cgs.debugLocation, doneLabel, cgs.debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "  %s:\n", doneLabel); err != nil {
			return err
		}
		if err := cgs.genGotoNext(w, cgs.nextIndex(), ""); err != nil {
			return err
		}

	case StatementResume:
		if _, err := fmt.Fprintf(w, "    ;RESUME\n"); err != nil {
			return err
		}
		argValIdent := ""
		if exprIdent, err := cgs.genExpr(w, cgs.stmt.Operands.(Expr)); err != nil {
			return err
		} else {
			argValIdent = exprIdent
		}
		if _, err := fmt.Fprintf(w, "    call void @check_error(%%val %s, i32 %d)%s\n", argValIdent, cgs.nextIndex(), cgs.debugLocation); err != nil {
			return err
		}
		argIdent := cgs.ident("arg")
		if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s, 1%s\n", argIdent, argValIdent, cgs.debugLocation); err != nil {
			return err
		}

		argiszeroIdent := cgs.ident("arg_is_zero")
		argiszeroLabel := cgs.label("stmt")
		argisnotzeroLabel := cgs.label("stmt")
		if _, err := fmt.Fprintf(w, "    %s = icmp eq i32 %s, 0%s\n", argiszeroIdent, argIdent, cgs.debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    br i1 %s, label %%%s, label %%%s%s\n", argiszeroIdent, argiszeroLabel, argisnotzeroLabel, cgs.debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "  %s:\n    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 621,1),i32 %d)%s\n    ret void%s\n", argiszeroLabel, cgs.nextIndex(), cgs.debugLocation, cgs.debugLocation); err != nil {
			return err
		}

		if _, err := fmt.Fprintf(w, "  %s:\n", argisnotzeroLabel); err != nil {
			return err
		}
		argTimesCountIdent := cgs.ident("arg_times_count")
		if _, err := fmt.Fprintf(w, "    %s = mul i32 %s,%s%s\n", argTimesCountIdent, argIdent, countIdent, cgs.debugLocation); err != nil {
			return err
		}
		stackptrIdent := cgs.ident("stackptr")
		checkunderflowIdent := cgs.ident("check_underflow")
		underflowLabel := cgs.label("stack_underflow")
		nounderflowLabel := cgs.label("stack_ok")
		if _, err := fmt.Fprintf(w, "    %s = load i32, i32* @stackptr%s\n", stackptrIdent, cgs.debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    %s = icmp ugt i32 %s, %s%s\n", checkunderflowIdent, argTimesCountIdent, stackptrIdent, cgs.debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    br i1 %s, label %%%s, label %%%s%s\n", checkunderflowIdent, underflowLabel, nounderflowLabel, cgs.debugLocation); err != nil {
			return err
		}

		doneLabel := cgs.label("resume_done")
		if _, err := fmt.Fprintf(w, "  %s:\n    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 632,1),i32 %d)%s\n    br label %%%s%s\n", underflowLabel, cgs.nextIndex(), cgs.debugLocation, doneLabel, cgs.debugLocation); err != nil {
			return err
		}

		newstackptrIdent := cgs.ident("new_stackptr")
		if _, err := fmt.Fprintf(w, "  %s:\n    %s = sub i32 %s, %s%s\n", nounderflowLabel, newstackptrIdent, stackptrIdent, argTimesCountIdent, cgs.debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    store i32 %s, i32* @stackptr%s\n", newstackptrIdent, cgs.debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    br label %%%s%s\n", doneLabel, cgs.debugLocation); err != nil {
			return err
		}

		if _, err := fmt.Fprintf(w, "  %s:\n", doneLabel); err != nil {
			return err
		}
		finalstackptrIdent := cgs.ident("final_stackptr")
		finalstackentryptrIdent := cgs.ident("final_stack_entry_ptr")
		nextStmtIdent := cgs.ident("next_stmt")
		if _, err := fmt.Fprintf(w, "    %s = load i32, i32* @stackptr%s\n", finalstackptrIdent, cgs.debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    %s = getelementptr [79 x i8*], [79 x i8*]* @stack,i32 0,i32 %s%s\n", finalstackentryptrIdent, finalstackptrIdent, cgs.debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    %s = load i8*, i8** %s%s\n", nextStmtIdent, finalstackentryptrIdent, cgs.debugLocation); err != nil {
			return err
		}

		if err774Ident != "" {
			err774Label := cgs.label("stmt")
			noErr774Label := cgs.label("stmt")
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s", err774Ident, err774Label, noErr774Label, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "  %s:\n    call void @fatal_error_on_the_way_to(i32 774,i8* %s)%s\n", err774Label, nextStmtIdent, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n", noErr774Label, cgs.debugLocation, noErr774Label); err != nil {
				return err
			}
		}

		if _, err := fmt.Fprintf(w, "    indirectbr i8* %s, [", nextStmtIdent); err != nil {
			return err
		}
		for i := range cgs.statements {
			if _, err := fmt.Fprintf(w, "label %%stmt%d,", i); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(w, "label %%stmt%d]%s\n", len(cgs.statements), cgs.debugLocation); err != nil {
			return err
		}

	case StatementStash:
		if _, err := fmt.Fprintf(w, "    ;STASH\n"); err != nil {
			return err
		}
		doneLabel := cgs.label("stash_done")
		enterLoopLabel := cgs.label("enter_loop")
		loopLabel := cgs.label("loop")
		loopEndLabel := cgs.label("loop_end")
		if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n    br label %%%s%s\n  %s:\n", enterLoopLabel, cgs.debugLocation, enterLoopLabel, loopLabel, cgs.debugLocation, loopLabel); err != nil {
			return err
		}
		if cgs.stmt.Chance > 100 {
			loopCounterIdent := cgs.ident("loop_counter")
			nextLoopCounterIdent := cgs.ident("next_loop_counter")
			if _, err := fmt.Fprintf(w, "    %s = phi i32 [0,%%%s],[%s,%%%s]%s\n", loopCounterIdent, enterLoopLabel, nextLoopCounterIdent, loopEndLabel, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    %s = add i32 %s,1%s\n", nextLoopCounterIdent, loopCounterIdent, cgs.debugLocation); err != nil {
				return err
			}
			loopCounterCheckIdent := cgs.ident("loop_counter_check")
			if _, err := fmt.Fprintf(w, "    %s = icmp uge i32 %s,%s%s\n", loopCounterCheckIdent, loopCounterIdent, countIdent, cgs.debugLocation); err != nil {
				return err
			}
			loopBodyLabel := cgs.label("loop_body")
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", loopCounterCheckIdent, doneLabel, loopBodyLabel, cgs.debugLocation, loopBodyLabel); err != nil {
			}
		}
		for _, operand := range cgs.stmt.Operands.([]Stashable) {
			varInfo := cgs.varInfo(operand)
			if !varInfo.retrieved {
				if _, err := fmt.Fprintf(w, "    ;%s never RETRIEVEd, not STASHING\n", varInfo.ident); err != nil {
					return err
				}
				continue
			}

			notignoredLabel := cgs.label("stmt")
			nextLabel := cgs.label("stmt")
			if ignoredIdent, err := cgs.genCheckIgnored(w, operand); err != nil {
				return err
			} else {
				if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", ignoredIdent, nextLabel, notignoredLabel, cgs.debugLocation, notignoredLabel); err != nil {
					return err
				}
			}

			switch operand.(type) {
			case Var16, Var32:
				if _, err := fmt.Fprintf(w, "    call void @stash_var(%s* %s)%s\n", varInfo.varType, varInfo.ident, cgs.debugLocation); err != nil {
					return err
				}
			case Array16, Array32:
				if _, err := fmt.Fprintf(w, "    call void @lazy_stash_arr(%s* %s)%s\n", varInfo.varType, varInfo.ident, cgs.debugLocation); err != nil {
					return err
				}
			default:
				panic("Stash")
			}

			if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n", nextLabel, cgs.debugLocation, nextLabel); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n", loopEndLabel, cgs.debugLocation, loopEndLabel); err != nil {
			return err
		}
		if cgs.stmt.Chance > 100 {
			if _, err := fmt.Fprintf(w, "    br label %%%s%s", loopLabel, cgs.debugLocation); err != nil {
				return err
			}
		} else {
			if _, err := fmt.Fprintf(w, "    br label %%%s%s", doneLabel, cgs.debugLocation); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(w, "  %s:\n", doneLabel); err != nil {
			return err
		}
		if err := cgs.genGotoNext(w, cgs.nextIndex(), ""); err != nil {
			return err
		}

	case StatementRetrieve:
		if _, err := fmt.Fprintf(w, "    ;RETRIEVE\n"); err != nil {
			return err
		}
		doneLabel := cgs.label("retrieve_done")
		enterLoopLabel := cgs.label("enter_loop")
		loopLabel := cgs.label("loop")
		loopEndLabel := cgs.label("loop_end")
		if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n    br label %%%s%s\n  %s:\n", enterLoopLabel, cgs.debugLocation, enterLoopLabel, loopLabel, cgs.debugLocation, loopLabel); err != nil {
			return err
		}
		if cgs.stmt.Chance > 100 {
			loopCounterIdent := cgs.ident("loop_counter")
			nextLoopCounterIdent := cgs.ident("next_loop_counter")
			if _, err := fmt.Fprintf(w, "    %s = phi i32 [0,%%%s],[%s,%%%s]%s\n", loopCounterIdent, enterLoopLabel, nextLoopCounterIdent, loopEndLabel, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    %s = add i32 %s,1%s\n", nextLoopCounterIdent, loopCounterIdent, cgs.debugLocation); err != nil {
				return err
			}
			loopCounterCheckIdent := cgs.ident("loop_counter_check")
			if _, err := fmt.Fprintf(w, "    %s = icmp uge i32 %s,%s%s\n", loopCounterCheckIdent, loopCounterIdent, countIdent, cgs.debugLocation); err != nil {
				return err
			}
			loopBodyLabel := cgs.label("loop_body")
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", loopCounterCheckIdent, doneLabel, loopBodyLabel, cgs.debugLocation, loopBodyLabel); err != nil {
			}
		}
		for _, operand := range cgs.stmt.Operands.([]Stashable) {
			varInfo := cgs.varInfo(operand)

			notignoredLabel := cgs.label("stmt")
			nextLabel := cgs.label("stmt")
			if ignoredIdent, err := cgs.genCheckIgnored(w, operand); err != nil {
				return err
			} else {
				if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", ignoredIdent, nextLabel, notignoredLabel, cgs.debugLocation, notignoredLabel); err != nil {
					return err
				}
			}

			retrievefailedLabel := cgs.label("stmt")
			retrievesucceededIdent := cgs.ident("retrieve_succeeded")

			switch operand.(type) {
			case Var16, Var32:
				if _, err := fmt.Fprintf(w, "    %s = call i1 @retrieve_var(%s* %s)%s\n", retrievesucceededIdent, varInfo.varType, varInfo.ident, cgs.debugLocation); err != nil {
					return err
				}
			case Array16, Array32:
				if _, err := fmt.Fprintf(w, "    %s = call i1 @lazy_retrieve_arr(%s* %s)%s\n", retrievesucceededIdent, varInfo.varType, varInfo.ident, cgs.debugLocation); err != nil {
					return err
				}
			default:
				panic("Retrieve")
			}

			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n", retrievesucceededIdent, nextLabel, retrievefailedLabel, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "  %s:\n    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 436,1),i32 %d)%s\n", retrievefailedLabel, cgs.nextIndex(), cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n", nextLabel, cgs.debugLocation, nextLabel); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n", loopEndLabel, cgs.debugLocation, loopEndLabel); err != nil {
			return err
		}
		if cgs.stmt.Chance > 100 {
			if _, err := fmt.Fprintf(w, "    br label %%%s%s", loopLabel, cgs.debugLocation); err != nil {
				return err
			}
		} else {
			if _, err := fmt.Fprintf(w, "    br label %%%s%s", doneLabel, cgs.debugLocation); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(w, "  %s:\n", doneLabel); err != nil {
			return err
		}
		if err := cgs.genGotoNext(w, cgs.nextIndex(), ""); err != nil {
			return err
		}

	case StatementIgnore, StatementRemember:
		flag := 1
		if cgs.stmt.Type == StatementIgnore {
			if _, err := fmt.Fprintf(w, "    ;IGNORE\n"); err != nil {
				return err
			}
		} else {
			flag = 0
			if _, err := fmt.Fprintf(w, "    ;REMEMBER\n"); err != nil {
				return err
			}
		}
		for _, operand := range cgs.stmt.Operands.([]Stashable) {
			varInfo := cgs.varInfo(operand)
			if _, err := fmt.Fprintf(w, "    store i1 %d,i1* getelementptr(%s,%s* %s,i32 0,i32 0)%s\n", flag, varInfo.varType, varInfo.varType, varInfo.ident, cgs.debugLocation); err != nil {
				return err
			}
		}
		if err := cgs.genGotoNext(w, cgs.nextIndex(), ""); err != nil {
			return err
		}

	case StatementAbstainLabel, StatementAbstainGerundList, StatementReinstateLabel, StatementReinstateGerundList:
		flag := 1
		if cgs.stmt.Type == StatementReinstateLabel || cgs.stmt.Type == StatementReinstateGerundList {
			flag = 0
			if _, err := fmt.Fprintf(w, "    ;REINSTATE\n"); err != nil {
				return err
			}
		} else {
			if _, err := fmt.Fprintf(w, "    ;ABSTAIN\n"); err != nil {
				return err
			}
		}
		for _, index := range cgs.stmt.Operands.([]int) {
			if _, err := fmt.Fprintf(w, "    store i1 %d, i1* getelementptr([%d x i1], [%d x i1]* @abstain_flags,i32 0,i32 %d)%s\n", flag, len(cgs.statements)+1, len(cgs.statements)+1, index, cgs.debugLocation); err != nil {
				return err
			}
		}
		if err := cgs.genGotoNext(w, cgs.nextIndex(), ""); err != nil {
			return err
		}

	case StatementGiveUp:
		if _, err := fmt.Fprintf(w, "    ;GIVE UP\n    call void @exit(i32 0) noreturn%s\n    ret void%s\n", cgs.debugLocation, cgs.debugLocation); err != nil {
			return err
		}

	case StatementReadOut:
		if _, err := fmt.Fprintf(w, "    ;READ OUT\n"); err != nil {
			return err
		}
		doneLabel := cgs.label("read_out_done")
		enterLoopLabel := cgs.label("enter_loop")
		loopLabel := cgs.label("loop")
		loopEndLabel := cgs.label("loop_end")
		if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n    br label %%%s%s\n  %s:\n", enterLoopLabel, cgs.debugLocation, enterLoopLabel, loopLabel, cgs.debugLocation, loopLabel); err != nil {
			return err
		}
		if cgs.stmt.Chance > 100 {
			loopCounterIdent := cgs.ident("loop_counter")
			nextLoopCounterIdent := cgs.ident("next_loop_counter")
			if _, err := fmt.Fprintf(w, "    %s = phi i32 [0,%%%s],[%s,%%%s]%s\n", loopCounterIdent, enterLoopLabel, nextLoopCounterIdent, loopEndLabel, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    %s = add i32 %s,1%s\n", nextLoopCounterIdent, loopCounterIdent, cgs.debugLocation); err != nil {
				return err
			}
			loopCounterCheckIdent := cgs.ident("loop_counter_check")
			if _, err := fmt.Fprintf(w, "    %s = icmp uge i32 %s,%s%s\n", loopCounterCheckIdent, loopCounterIdent, countIdent, cgs.debugLocation); err != nil {
				return err
			}
			loopBodyLabel := cgs.label("loop_body")
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", loopCounterCheckIdent, doneLabel, loopBodyLabel, cgs.debugLocation, loopBodyLabel); err != nil {
			}
		}
		for _, arg := range cgs.stmt.Operands.([]ReadOutable) {
			exprIdent, err := cgs.genExpr(w, arg)
			if err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    call void @check_error(%%val %s, i32 %d)%s\n", exprIdent, cgs.nextIndex(), cgs.debugLocation); err != nil {
				return err
			}
			valIdent := cgs.ident("val")
			if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s, 1%s\n    call void @output(i32 %s)%s\n", valIdent, exprIdent, cgs.debugLocation, valIdent, cgs.debugLocation); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n", loopEndLabel, cgs.debugLocation, loopEndLabel); err != nil {
			return err
		}
		if cgs.stmt.Chance > 100 {
			if _, err := fmt.Fprintf(w, "    br label %%%s%s", loopLabel, cgs.debugLocation); err != nil {
				return err
			}
		} else {
			if _, err := fmt.Fprintf(w, "    br label %%%s%s", doneLabel, cgs.debugLocation); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(w, "  %s:\n", doneLabel); err != nil {
			return err
		}
		if err := cgs.genGotoNext(w, cgs.nextIndex(), ""); err != nil {
			return err
		}

	case StatementWriteIn:
		if _, err := fmt.Fprintf(w, "    ;WRITE IN\n"); err != nil {
			return err
		}
		doneLabel := cgs.label("write_in_done")
		enterLoopLabel := cgs.label("enter_loop")
		loopLabel := cgs.label("loop")
		loopEndLabel := cgs.label("loop_end")
		if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n    br label %%%s%s\n  %s:\n", enterLoopLabel, cgs.debugLocation, enterLoopLabel, loopLabel, cgs.debugLocation, loopLabel); err != nil {
			return err
		}
		if cgs.stmt.Chance > 100 {
			loopCounterIdent := cgs.ident("loop_counter")
			nextLoopCounterIdent := cgs.ident("next_loop_counter")
			if _, err := fmt.Fprintf(w, "    %s = phi i32 [0,%%%s],[%s,%%%s]%s\n", loopCounterIdent, enterLoopLabel, nextLoopCounterIdent, loopEndLabel, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    %s = add i32 %s,1%s\n", nextLoopCounterIdent, loopCounterIdent, cgs.debugLocation); err != nil {
				return err
			}
			loopCounterCheckIdent := cgs.ident("loop_counter_check")
			if _, err := fmt.Fprintf(w, "    %s = icmp uge i32 %s,%s%s\n", loopCounterCheckIdent, loopCounterIdent, countIdent, cgs.debugLocation); err != nil {
				return err
			}
			loopBodyLabel := cgs.label("loop_body")
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", loopCounterCheckIdent, doneLabel, loopBodyLabel, cgs.debugLocation, loopBodyLabel); err != nil {
			}
		}
		for _, vrbl := range cgs.stmt.Operands.([]WriteInable) {
			ignoredIdent, err := cgs.genCheckIgnored(w, vrbl)
			if err != nil {
				return err
			}
			writeinLabel := cgs.label("stmt")
			nextLabel := cgs.label("stmt")
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", ignoredIdent, nextLabel, writeinLabel, cgs.debugLocation, writeinLabel); err != nil {
				return err
			}

			switch v := vrbl.(type) {
			case Var16, Var32, ArrayElement:
				inputIdent := "@input32"
				if v.Is16() {
					inputIdent = "@input16"
				}
				inputvalIdent := cgs.ident("input_val")
				if _, err := fmt.Fprintf(w, "    %s = call %%val %s()%s\n", inputvalIdent, inputIdent, cgs.debugLocation); err != nil {
					return err
				}

				if err := cgs.genGets(w, v.(LValue), inputvalIdent, nextLabel); err != nil {
					return err
				}

			case Array16, Array32:
				varInfo := cgs.varInfo(v)
				writeinCall := ""
				if v.Is16() {
					writeinCall = fmt.Sprintf("@writein16(%s* %s)", varInfo.varType, varInfo.ident)
				} else {
					writeinCall = fmt.Sprintf("@writein32(%s* %s)", varInfo.varType, varInfo.ident)
				}

				errIdent := cgs.ident("write_in_err")
				if _, err := fmt.Fprintf(w, "    %s = call %%val %s%s\n", errIdent, writeinCall, cgs.debugLocation); err != nil {
					return err
				}
				if _, err := fmt.Fprintf(w, "    call void @check_error(%%val %s, i32 %d)%s\n", errIdent, cgs.nextIndex(), cgs.debugLocation); err != nil {
					return err
				}
				if _, err := fmt.Fprintf(w, "    br label %%%s%s\n", nextLabel, cgs.debugLocation); err != nil {
					return err
				}

			default:
				if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 778,1),i32 %d)%s\n    br label %s%s\n", cgs.nextIndex(), cgs.debugLocation, nextLabel, cgs.debugLocation); err != nil {
					return err
				}
			}

			if _, err := fmt.Fprintf(w, "  %s:\n", nextLabel); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n", loopEndLabel, cgs.debugLocation, loopEndLabel); err != nil {
			return err
		}
		if cgs.stmt.Chance > 100 {
			if _, err := fmt.Fprintf(w, "    br label %%%s%s", loopLabel, cgs.debugLocation); err != nil {
				return err
			}
		} else {
			if _, err := fmt.Fprintf(w, "    br label %%%s%s", doneLabel, cgs.debugLocation); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(w, "  %s:\n", doneLabel); err != nil {
			return err
		}
		if err := cgs.genGotoNext(w, cgs.nextIndex(), ""); err != nil {
			return err
		}

	case StatementReadOutBit:
		bit := "0"
		if cgs.stmt.Operands.(bool) {
			bit = "1"
		}
		if _, err := fmt.Fprintf(w, "    ;READ OUT\n"); err != nil {
			return err
		}
		doneLabel := cgs.label("read_out_done")
		enterLoopLabel := cgs.label("enter_loop")
		loopLabel := cgs.label("loop")
		loopEndLabel := cgs.label("loop_end")
		if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n    br label %%%s%s\n  %s:\n", enterLoopLabel, cgs.debugLocation, enterLoopLabel, loopLabel, cgs.debugLocation, loopLabel); err != nil {
			return err
		}
		if cgs.stmt.Chance > 100 {
			loopCounterIdent := cgs.ident("loop_counter")
			nextLoopCounterIdent := cgs.ident("next_loop_counter")
			if _, err := fmt.Fprintf(w, "    %s = phi i32 [0,%%%s],[%s,%%%s]%s\n", loopCounterIdent, enterLoopLabel, nextLoopCounterIdent, loopEndLabel, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    %s = add i32 %s,1%s\n", nextLoopCounterIdent, loopCounterIdent, cgs.debugLocation); err != nil {
				return err
			}
			loopCounterCheckIdent := cgs.ident("loop_counter_check")
			if _, err := fmt.Fprintf(w, "    %s = icmp uge i32 %s,%s%s\n", loopCounterCheckIdent, loopCounterIdent, countIdent, cgs.debugLocation); err != nil {
				return err
			}
			loopBodyLabel := cgs.label("loop_body")
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", loopCounterCheckIdent, doneLabel, loopBodyLabel, cgs.debugLocation, loopBodyLabel); err != nil {
			}
		}
		if _, err := fmt.Fprintf(w, "    call void @output_binary(i1 %s)%s\n", bit, cgs.debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n", loopEndLabel, cgs.debugLocation, loopEndLabel); err != nil {
			return err
		}
		if cgs.stmt.Chance > 100 {
			if _, err := fmt.Fprintf(w, "    br label %%%s%s", loopLabel, cgs.debugLocation); err != nil {
				return err
			}
		} else {
			if _, err := fmt.Fprintf(w, "    br label %%%s%s", doneLabel, cgs.debugLocation); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(w, "  %s:\n", doneLabel); err != nil {
			return err
		}
		if err := cgs.genGotoNext(w, cgs.nextIndex(), ""); err != nil {
			return err
		}

	case StatementWriteIntoBit:
		if _, err := fmt.Fprintf(w, "    ;WRITE IN\n"); err != nil {
			return err
		}

		enterLoopLabel := cgs.label("enter_loop")
		if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n", enterLoopLabel, cgs.debugLocation, enterLoopLabel); err != nil {
			return err
		}
		loopLabel := cgs.label("loop")
		loopEndLabel := cgs.label("loop_end")
		if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n", loopLabel, cgs.debugLocation, loopLabel); err != nil {
			return err
		}
		loopCounterIdent := cgs.ident("loop_counter")
		nextLoopCounterIdent := cgs.ident("next_loop_counter")
		if cgs.stmt.Chance > 100 {
			if _, err := fmt.Fprintf(w, "    %s = phi i32 [0,%%%s],[%s,%%%s]%s\n", loopCounterIdent, enterLoopLabel, nextLoopCounterIdent, loopEndLabel, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    %s = add i32 %s,1%s\n", nextLoopCounterIdent, loopCounterIdent, cgs.debugLocation); err != nil {
				return err
			}
		}
		inputIdent := cgs.ident("input")
		if _, err := fmt.Fprintf(w, "    %s = call {i1,i1} @input_binary()%s\n", inputIdent, cgs.debugLocation); err != nil {
			return err
		}
		inputeofIdent := cgs.ident("input_eof")
		if _, err := fmt.Fprintf(w, "    %s = extractvalue {i1,i1} %s,1%s\n", inputeofIdent, inputIdent, cgs.debugLocation); err != nil {
			return err
		}
		gotEofLabel := cgs.label("got_eof")
		if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n", inputeofIdent, gotEofLabel, loopEndLabel, cgs.debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "  %s:\n", loopEndLabel); err != nil {
			return err
		}
		checkbitLabel := cgs.label("check_bit")
		if cgs.stmt.Chance > 100 {
			loopCounterCheckIdent := cgs.ident("loop_counter_check")
			if _, err := fmt.Fprintf(w, "    %s = icmp uge i32 %s,%s%s\n", loopCounterCheckIdent, loopCounterIdent, countIdent, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n", loopCounterCheckIdent, checkbitLabel, loopLabel, cgs.debugLocation); err != nil {
				return err
			}
		} else {
			if _, err := fmt.Fprintf(w, "    br label %%%s%s\n", checkbitLabel, cgs.debugLocation); err != nil {
				return err
			}
		}

		inputbitIdent := cgs.ident("input_bit")
		if _, err := fmt.Fprintf(w, "  %s:\n    %s = extractvalue {i1,i1} %s,0%s\n", checkbitLabel, inputbitIdent, inputIdent, cgs.debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    br i1 %s,label %%stmt%d,label %%stmt%d%s\n", inputbitIdent, cgs.stmt.Operands.([2]int)[1], cgs.stmt.Operands.([2]int)[0], cgs.debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "  %s:\n", gotEofLabel); err != nil {
			return err
		}
		if err := cgs.genGotoNext(w, cgs.nextIndex(), ""); err != nil {
			return err
		}

	case StatementLibrary:
		if _, err := fmt.Fprintf(w, "    ;NEXT\n"); err != nil {
			return err
		}
		doneLabel := cgs.label("next_done")
		enterLoopLabel := cgs.label("enter_loop")
		loopLabel := cgs.label("loop")
		loopEndLabel := cgs.label("loop_end")
		if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n    br label %%%s%s\n  %s:\n", enterLoopLabel, cgs.debugLocation, enterLoopLabel, loopLabel, cgs.debugLocation, loopLabel); err != nil {
			return err
		}
		if cgs.stmt.Chance > 100 {
			loopCounterIdent := cgs.ident("loop_counter")
			nextLoopCounterIdent := cgs.ident("next_loop_counter")
			if _, err := fmt.Fprintf(w, "    %s = phi i32 [0,%%%s],[%s,%%%s]%s\n", loopCounterIdent, enterLoopLabel, nextLoopCounterIdent, loopEndLabel, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    %s = add i32 %s,1%s\n", nextLoopCounterIdent, loopCounterIdent, cgs.debugLocation); err != nil {
				return err
			}
			loopCounterCheckIdent := cgs.ident("loop_counter_check")
			if _, err := fmt.Fprintf(w, "    %s = icmp uge i32 %s,%s%s\n", loopCounterCheckIdent, loopCounterIdent, countIdent, cgs.debugLocation); err != nil {
				return err
			}
			loopBodyLabel := cgs.label("loop_body")
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", loopCounterCheckIdent, doneLabel, loopBodyLabel, cgs.debugLocation, loopBodyLabel); err != nil {
			}
		}
		cgs.stmt.Operands.(LibraryFunction).CodeGen(w, cgs)
		if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n", loopEndLabel, cgs.debugLocation, loopEndLabel); err != nil {
			return err
		}
		if cgs.stmt.Chance > 100 {
			if _, err := fmt.Fprintf(w, "    br label %%%s%s", loopLabel, cgs.debugLocation); err != nil {
				return err
			}
		} else {
			if _, err := fmt.Fprintf(w, "    br label %%%s%s", doneLabel, cgs.debugLocation); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(w, "  %s:\n", doneLabel); err != nil {
			return err
		}
		if err := cgs.genGotoNext(w, cgs.nextIndex(), ""); err != nil {
			return err
		}

	case StatementComeFromLabel, StatementComeFromGerundList, StatementNextFromLabel, StatementNextFromGerundList:
		if err := cgs.genGotoNext(w, cgs.nextIndex(), ""); err != nil {
			return err
		}

	default:
		if _, err := fmt.Fprintf(w, "    ;UNKNOWN ERROR\n    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 778,1),i32 %d)%s\n    ret void%s\n", cgs.nextIndex(), cgs.debugLocation, cgs.debugLocation); err != nil {
			return err
		}
	}
	return nil
}

func (cgs *codeGenState) genCheckIgnored(w io.Writer, ignoredable Ignoredable) (string, error) {
	varInfo := cgs.varInfo(ignoredable)
	if !varInfo.ignored {
		neverIgnoredIdent := cgs.ident("never_ignored")
		if _, err := fmt.Fprintf(w, "    %s = select i1 1,i1 0,i1 0%s\n", neverIgnoredIdent, cgs.debugLocation); err != nil {
			return "", err
		}
		return neverIgnoredIdent, nil
	}

	ignoredptrIdent := cgs.ident("ignored_ptr")
	ignoredIdent := cgs.ident("ignored")
	if _, err := fmt.Fprintf(w, "    %s = getelementptr %s, %s* %s, i32 0, i32 0%s\n", ignoredptrIdent, varInfo.varType, varInfo.varType, varInfo.ident, cgs.debugLocation); err != nil {
		return "", err
	}
	if _, err := fmt.Fprintf(w, "    %s = load i1,i1* %s%s\n", ignoredIdent, ignoredptrIdent, cgs.debugLocation); err != nil {
		return "", err
	}

	return ignoredIdent, nil
}

func (cgs *codeGenState) genExpr(w io.Writer, expr Expr) (string, error) {
	ident := cgs.ident("expr")

	if val, is16, isConst := expr.ConstValue(); isConst {
		tag := 0
		if !is16 {
			tag = 1
		}
		if _, err := fmt.Fprintf(w, "    %s = select i1 1, %%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 %d,0),i32 %d,1), %%val zeroinitializer%s\n", ident, tag, val, cgs.debugLocation); err != nil {
			return "", nil
		}
		return ident, nil
	}

	switch e := expr.(type) {
	case Var16:
		if err := cgs.genAccessVar(w, ident, cgs.varInfo(e)); err != nil {
			return "", err
		} else {
			return ident, nil
		}

	case Var32:
		if err := cgs.genAccessVar(w, ident, cgs.varInfo(e)); err != nil {
			return "", err
		} else {
			return ident, nil
		}

	case ArrayElement:
		if err := cgs.genAccessArrayElt(w, ident, e); err != nil {
			return "", err
		} else {
			return ident, nil
		}

	case ExprConst:
		panic("ExprConst")

	case ExprMingle:
		leftIdent, err := cgs.genExpr(w, e[0])
		if err != nil {
			return "", err
		}
		rightIdent, err := cgs.genExpr(w, e[1])
		if err != nil {
			return "", err
		}
		if _, err := fmt.Fprintf(w, "    %s = call %%val @op_mingle(%%val %s,%%val %s)%s\n", ident, leftIdent, rightIdent, cgs.debugLocation); err != nil {
			return "", err
		}
		return ident, nil

	case ExprSelect:
		leftIdent, err := cgs.genExpr(w, e[0])
		if err != nil {
			return "", err
		}
		rightIdent, err := cgs.genExpr(w, e[1])
		if err != nil {
			return "", err
		}
		if _, err := fmt.Fprintf(w, "    %s = call %%val @op_select(%%val %s,%%val %s)%s\n", ident, leftIdent, rightIdent, cgs.debugLocation); err != nil {
			return "", err
		}
		return ident, nil

	case ExprAnd:
		operandIdent, err := cgs.genExpr(w, e[0])
		if err != nil {
			return "", err
		}
		if _, err := fmt.Fprintf(w, "    %s = call %%val @op_and(%%val %s)%s\n", ident, operandIdent, cgs.debugLocation); err != nil {
			return "", err
		}
		return ident, nil

	case ExprOr:
		operandIdent, err := cgs.genExpr(w, e[0])
		if err != nil {
			return "", err
		}
		if _, err := fmt.Fprintf(w, "    %s = call %%val @op_or(%%val %s)%s\n", ident, operandIdent, cgs.debugLocation); err != nil {
			return "", err
		}
		return ident, nil

	case ExprXor:
		operandIdent, err := cgs.genExpr(w, e[0])
		if err != nil {
			return "", err
		}
		if _, err := fmt.Fprintf(w, "    %s = call %%val @op_xor(%%val %s)%s\n", ident, operandIdent, cgs.debugLocation); err != nil {
			return "", err
		}
		return ident, nil

	default:
		error778Ident := cgs.ident("error778_")
		if _, err := fmt.Fprintf(w, "    %s = select i1 1, %%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 778,1), %%val zeroinitializer%s\n", error778Ident, cgs.debugLocation); err != nil {
			return "", nil
		}
		return error778Ident, nil
	}
}

func (cgs *codeGenState) genGets(w io.Writer, lhs LValue, rhsIdent string, doneLabel string) error {
	if _, err := fmt.Fprintf(w, "    call void @check_error(%%val %s, i32 %d)%s\n", rhsIdent, cgs.nextIndex(), cgs.debugLocation); err != nil {
		return err
	}

	rhsi32valueIdent := cgs.ident("rhs_i32_value")
	if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", rhsi32valueIdent, rhsIdent, cgs.debugLocation); err != nil {
		return err
	}

	if lhs.Is16() {
		rhstagIdent := cgs.ident("rhs_tag")
		rhstagisoneIdent := cgs.ident("rhs_tag_is_one")
		dorangecheckLabel := cgs.label("stmt")
		rangeokLabel := cgs.label("stmt")
		if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,0%s\n", rhstagIdent, rhsIdent, cgs.debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    %s = icmp eq i2 %s,1%s\n", rhstagisoneIdent, rhstagIdent, cgs.debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n", rhstagisoneIdent, dorangecheckLabel, rangeokLabel, cgs.debugLocation); err != nil {
			return err
		}

		rhs65535checkIdent := cgs.ident("rhs_65535_check")
		failrangecheckLabel := cgs.label("stmt")
		if _, err := fmt.Fprintf(w, "  %s:\n    %s = icmp uge i32 %s,65535%s ; uge and not ugt: 4.4.1 says, '16-bit variables may be assigned 32-bit values only if the value is less than 65535.'\n", dorangecheckLabel, rhs65535checkIdent, rhsi32valueIdent, cgs.debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n", rhs65535checkIdent, failrangecheckLabel, rangeokLabel, cgs.debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "  %s:\n    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 275,1),i32 %d)%s\n", failrangecheckLabel, cgs.nextIndex(), cgs.debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n", rangeokLabel, cgs.debugLocation, rangeokLabel); err != nil {
			return err
		}
	}

	varInfo := cgs.varInfo(lhs)
	switch lhs.(type) {
	case Var16, Var32:
		valueptrptrIdent := cgs.ident("valueptr_ptr")
		valueptrIdent := cgs.ident("valueptr")
		if _, err := fmt.Fprintf(w, "    %s = getelementptr %%vrbl, %%vrbl* %s,i32 0,i32 1%s\n", valueptrptrIdent, varInfo.ident, cgs.debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    %s = load %%vrbl_val*, %%vrbl_val** %s%s\n", valueptrIdent, valueptrptrIdent, cgs.debugLocation); err != nil {
			return err
		}

		// store value
		i32valueptrIdent := cgs.ident("i32_valueptr")
		if _, err := fmt.Fprintf(w, "    %s = getelementptr %%vrbl_val, %%vrbl_val* %s, i32 0, i32 2%s\n", i32valueptrIdent, valueptrIdent, cgs.debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    store i32 %s,i32* %s%s\n    br label %%%s%s\n", rhsi32valueIdent, i32valueptrIdent, cgs.debugLocation, doneLabel, cgs.debugLocation); err != nil {
			return err
		}
		return nil

	case ArrayElement:
		if varInfo.retrieved {
			if _, err := fmt.Fprintf(w, "    call %%arr_val* @lazy_stash_arr_cow(%%arr_vrbl* %s,i1 1)%s\n", varInfo.ident, cgs.debugLocation); err != nil {
				return err
			}
		}

		indexErrorLabel := cgs.label("stmt")
		boundsErrorLabel := cgs.label("stmt")

		indexIdentSrcLabels, arrayElementPtrIdent, err := cgs.genFindArrayElement(w, boundsErrorLabel, indexErrorLabel, lhs.(ArrayElement))
		if err != nil {
			return err
		}

		if _, err := fmt.Fprintf(w, "    store i32 %s,i32* %s%s\n", rhsi32valueIdent, arrayElementPtrIdent, cgs.debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "    br label %%%s%s\n", doneLabel, cgs.debugLocation); err != nil {
			return err
		}

		if _, err := fmt.Fprintf(w, "  %s:\n    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 241,1),i32 %d)%s\n    br label %%%s%s\n", boundsErrorLabel, cgs.nextIndex(), cgs.debugLocation, doneLabel, cgs.debugLocation); err != nil {
			return err
		}

		indexerrorIdent := cgs.ident("index_error")
		if _, err := fmt.Fprintf(w, "    %s:\n    %s = phi %%val ", indexErrorLabel, indexerrorIdent); err != nil {
			return err
		}
		for i, indexIdentSrcLabel := range indexIdentSrcLabels {
			comma := ","
			if i == 0 {
				comma = ""
			}
			if _, err := fmt.Fprintf(w, "%s[%s,%%%s]", comma, indexIdentSrcLabel[0], indexIdentSrcLabel[1]); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(w, "    call void @fatal_error(%%val %s,i32 %d)%s\n    br label %%%s%s\n", indexerrorIdent, cgs.nextIndex(), cgs.debugLocation, doneLabel, cgs.debugLocation); err != nil {
			return err
		}

		return nil

	default:
		panic("Gets")
	}
}

func (cgs *codeGenState) genAccessVar(w io.Writer, resultIdent string, varInfo *codeGenVar) error {
	if !varInfo.assigned {
		tag := 0
		if !varInfo.is16 {
			tag = 1
		}
		if _, err := fmt.Fprintf(w, "    %s = select i1 1,%%val insertvalue(%%val zeroinitializer,i2 %d,0),%%val zeroinitializer%s\n", resultIdent, tag, cgs.debugLocation); err != nil {
			return err
		}
		return nil
	}

	accessLabel := cgs.label("stmt")
	if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s: ; access %s\n", accessLabel, cgs.debugLocation, accessLabel, varInfo.ident); err != nil {
		return err
	}

	valptrIdent := cgs.ident("val_ptr")
	if _, err := fmt.Fprintf(w, "    %s = load %%vrbl_val*, %%vrbl_val** getelementptr(%%vrbl, %%vrbl* %s, i32 0, i32 1)%s\n", valptrIdent, varInfo.ident, cgs.debugLocation); err != nil {
		return err
	}

	valptrisnullIdent := cgs.ident("val_ptr_is_null")
	collectResultLabel := cgs.label("stmt")
	loadResultValueLabel := cgs.label("stmt")
	if _, err := fmt.Fprintf(w, "    %s = icmp eq %%vrbl_val* %s, null%s\n", valptrisnullIdent, valptrIdent, cgs.debugLocation); err != nil {
		return err
	}
	if _, err := fmt.Fprintf(w, "    br i1 %s, label %%%s, label %%%s%s\n", valptrisnullIdent, collectResultLabel, loadResultValueLabel, cgs.debugLocation); err != nil {
		return err
	}

	loadresultptrIdent := cgs.ident("load_result_ptr")
	loadedresultIdent := cgs.ident("loaded_result")
	if _, err := fmt.Fprintf(w, "  %s:\n    %s = getelementptr %%vrbl_val, %%vrbl_val* %s,i32 0,i32 2%s\n", loadResultValueLabel, loadresultptrIdent, valptrIdent, cgs.debugLocation); err != nil {
		return err
	}
	if _, err := fmt.Fprintf(w, "    %s = load i32, i32* %s%s\n", loadedresultIdent, loadresultptrIdent, cgs.debugLocation); err != nil {
		return err
	}
	if _, err := fmt.Fprintf(w, "  br label %%%s%s\n", collectResultLabel, cgs.debugLocation); err != nil {
		return err
	}

	collectedresultIdent := cgs.ident("collected_result")
	if _, err := fmt.Fprintf(w, "  %s:\n    %s = phi i32 [0,%%%s],[%s,%%%s]%s\n", collectResultLabel, collectedresultIdent, accessLabel, loadedresultIdent, loadResultValueLabel, cgs.debugLocation); err != nil {
		return err
	}
	tag := 0
	if !varInfo.is16 {
		tag = 1
	}
	if _, err := fmt.Fprintf(w, "    %s = insertvalue %%val insertvalue(%%val zeroinitializer,i2 %d,0),i32 %s,1%s\n", resultIdent, tag, collectedresultIdent, cgs.debugLocation); err != nil {
		return err
	}
	return nil
}

func (cgs *codeGenState) genAccessArrayElt(w io.Writer, resultIdent string, arrayElement ArrayElement) error {
	collectValueLabel := cgs.label("stmt")
	boundsErrorLabel := cgs.label("stmt")
	loadvalueLabel := cgs.label("stmt")

	indexIdentSrcLabels, arrayElementPtrIdent, err := cgs.genFindArrayElement(w, boundsErrorLabel, collectValueLabel, arrayElement)
	if err != nil {
		return err
	}

	if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n", loadvalueLabel, cgs.debugLocation, loadvalueLabel); err != nil {
		return err
	}
	loadedvalueIdent := cgs.ident("loaded_value")
	loadedvaluevalIdent := cgs.ident("loaded_value_val")
	if _, err := fmt.Fprintf(w, "    %s = load i32,i32* %s%s\n", loadedvalueIdent, arrayElementPtrIdent, cgs.debugLocation); err != nil {
		return err
	}
	tag := 0
	if _, ok := arrayElement.Array.(Array32); ok {
		tag = 1
	}
	if _, err := fmt.Fprintf(w, "    %s = insertvalue %%val insertvalue(%%val zeroinitializer,i2 %d,0),i32 %s,1%s\n", loadedvaluevalIdent, tag, loadedvalueIdent, cgs.debugLocation); err != nil {
		return err
	}
	if _, err := fmt.Fprintf(w, "    br label %%%s%s\n", collectValueLabel, cgs.debugLocation); err != nil {
		return err
	}

	if _, err := fmt.Fprintf(w, "  %s:\n    br label %%%s%s\n", boundsErrorLabel, collectValueLabel, cgs.debugLocation); err != nil {
		return err
	}

	if _, err := fmt.Fprintf(w, "  %s:\n    %s = phi %%val [insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 241,1),%%%s]", collectValueLabel, resultIdent, boundsErrorLabel); err != nil {
		return err
	}
	// errors in calculating indexes
	for _, indexIdentSrcLabel := range indexIdentSrcLabels {
		if _, err := fmt.Fprintf(w, ",[%s,%%%s]", indexIdentSrcLabel[0], indexIdentSrcLabel[1]); err != nil {
			return err
		}
	}
	// result from successfully loading array element
	if _, err := fmt.Fprintf(w, ",[%s,%%%s]%s\n", loadedvaluevalIdent, loadvalueLabel, cgs.debugLocation); err != nil {
		return err
	}
	return nil
}

// returns [][2]string{[2]string{indexIdent(%val),indexErrorLabel(label)}}, arrayElementPtrIdent(i32*), error
func (cgs *codeGenState) genFindArrayElement(w io.Writer, boundsErrorLabel, indexErrorLabel string, arrayElement ArrayElement) ([][2]string, string, error) {
	varInfo := cgs.varInfo(arrayElement)
	valptrIdent := cgs.ident("val_ptr")
	if _, err := fmt.Fprintf(w, "    %s = load %%arr_val*, %%arr_val** getelementptr(%%arr_vrbl, %%arr_vrbl* %s, i32 0, i32 1)%s\n", valptrIdent, varInfo.ident, cgs.debugLocation); err != nil {
		return nil, "", err
	}

	// check for uninitialized array
	valptrisnullIdent := cgs.ident("val_ptr_is_null")
	checkDimsLabel := cgs.label("stmt")
	if _, err := fmt.Fprintf(w, "    %s = icmp eq %%arr_val* %s, null%s\n", valptrisnullIdent, valptrIdent, cgs.debugLocation); err != nil {
		return nil, "", err
	}
	if _, err := fmt.Fprintf(w, "    br i1 %s, label %%%s, label %%%s%s\n", valptrisnullIdent, boundsErrorLabel, checkDimsLabel, cgs.debugLocation); err != nil {
		return nil, "", err
	}

	// check dimensions match
	if _, err := fmt.Fprintf(w, "  %s:\n", checkDimsLabel); err != nil {
		return nil, "", err
	}
	dimsptrIdent := cgs.ident("dims_ptr")
	dimsIdent := cgs.ident("dims")
	dimscheckIdent := cgs.ident("dims_check")
	getIndexesLabel := cgs.label("stmt")
	if _, err := fmt.Fprintf(w, "    %s = getelementptr %%arr_val, %%arr_val* %s,i32 0,i32 2%s\n", dimsptrIdent, valptrIdent, cgs.debugLocation); err != nil {
		return nil, "", err
	}
	if _, err := fmt.Fprintf(w, "    %s = load i32, i32* %s%s\n", dimsIdent, dimsptrIdent, cgs.debugLocation); err != nil {
		return nil, "", err
	}
	if _, err := fmt.Fprintf(w, "    %s = icmp eq i32 %s,%d%s\n", dimscheckIdent, dimsIdent, len(arrayElement.Index), cgs.debugLocation); err != nil {
		return nil, "", err
	}
	if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n", dimscheckIdent, getIndexesLabel, boundsErrorLabel, cgs.debugLocation); err != nil {
		return nil, "", err
	}

	// get indexes
	if _, err := fmt.Fprintf(w, "  %s:\n", getIndexesLabel); err != nil {
		return nil, "", err
	}
	indexIdentSrcLabels := [][2]string{} // {indexExprValIdent,label}
	for i, indexExpr := range arrayElement.Index {
		indexIdent, err := cgs.genExpr(w, indexExpr)
		if err != nil {
			return nil, "", err
		}
		indexSrcLabel := cgs.label("stmt")
		indexIdentSrcLabels = append(indexIdentSrcLabels, [2]string{indexIdent, indexSrcLabel})

		if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s: ;index %d error check\n", indexSrcLabel, cgs.debugLocation, indexSrcLabel, i); err != nil {
			return nil, "", err
		}

		indextagIdent := cgs.ident("index_tag")
		indextagcheckIdent := cgs.ident("index_tag_check")
		if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,0%s\n    %s = icmp uge i2 %s,2%s\n", indextagIdent, indexIdent, cgs.debugLocation, indextagcheckIdent, indextagIdent, cgs.debugLocation); err != nil {
			return nil, "", err
		}

		nextIndexLabel := cgs.label("stmt")
		if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", indextagcheckIdent, indexErrorLabel, nextIndexLabel, cgs.debugLocation, nextIndexLabel); err != nil {
			return nil, "", err
		}
	}

	// check array bounds and calculate index to load from
	currentindextotalIdent := ""
	currentindexmultiplierIdent := ""
	for i, indexIdentSrcLabel := range indexIdentSrcLabels {
		currentindexIdent := cgs.ident("current_index")
		zerocheckIdent := cgs.ident("zero_check")
		zerocheckOkLabel := cgs.label("stmt")
		if _, err := fmt.Fprintf(w, "    %s = extractvalue %%val %s,1%s\n", currentindexIdent, indexIdentSrcLabel[0], cgs.debugLocation); err != nil {
			return nil, "", err
		}
		if _, err := fmt.Fprintf(w, "    %s = icmp eq i32 %s,0%s\n", zerocheckIdent, currentindexIdent, cgs.debugLocation); err != nil {
			return nil, "", err
		}
		if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", zerocheckIdent, boundsErrorLabel, zerocheckOkLabel, cgs.debugLocation, zerocheckOkLabel); err != nil {
			return nil, "", err
		}

		currentindexboundptrIdent := cgs.ident("current_index_bound_ptr")
		currentindexboundIdent := cgs.ident("current_index_bound")
		if _, err := fmt.Fprintf(w, "    %s = getelementptr %%arr_val, %%arr_val* %s,i32 0,i32 3,i32 %d%s\n", currentindexboundptrIdent, valptrIdent, i, cgs.debugLocation); err != nil {
			return nil, "", err
		}
		if _, err := fmt.Fprintf(w, "    %s = load i32,i32* %s%s\n", currentindexboundIdent, currentindexboundptrIdent, cgs.debugLocation); err != nil {
			return nil, "", err
		}

		outofboundscheckIdent := cgs.ident("out_of_bound_check")
		boundscheckOkLabel := cgs.label("stmt")
		if _, err := fmt.Fprintf(w, "    %s = icmp ugt i32 %s,%s%s\n", outofboundscheckIdent, currentindexIdent, currentindexboundIdent, cgs.debugLocation); err != nil {
			return nil, "", err
		}
		if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", outofboundscheckIdent, boundsErrorLabel, boundscheckOkLabel, cgs.debugLocation, boundscheckOkLabel); err != nil {
			return nil, "", err
		}

		if i == 0 {
			currentindextotalIdent = cgs.ident("current_index_total")
			currentindexmultiplierIdent = currentindexboundIdent
			if _, err := fmt.Fprintf(w, "    %s = sub i32 %s,1%s\n", currentindextotalIdent, currentindexIdent, cgs.debugLocation); err != nil {
				return nil, "", err
			}
		} else {
			currentindexminusoneIdent := cgs.ident("current_index_minus_one")
			currentindexminusonetimesmultIdent := cgs.ident("current_index_minus_one_times_mult")
			newcurrentindextotalIdent := cgs.ident("new_current_index_total")
			newcurrentindexmultiplierIdent := cgs.ident("new_current_index_multiplier")
			if _, err := fmt.Fprintf(w, "    %s = sub i32 %s,1%s\n", currentindexminusoneIdent, currentindexIdent, cgs.debugLocation); err != nil {
				return nil, "", err
			}
			if _, err := fmt.Fprintf(w, "    %s = mul i32 %s,%s%s\n", currentindexminusonetimesmultIdent, currentindexminusoneIdent, currentindexmultiplierIdent, cgs.debugLocation); err != nil {
				return nil, "", err
			}
			if _, err := fmt.Fprintf(w, "    %s = add i32 %s,%s%s\n", newcurrentindextotalIdent, currentindexminusonetimesmultIdent, currentindextotalIdent, cgs.debugLocation); err != nil {
				return nil, "", err
			}
			if _, err := fmt.Fprintf(w, "    %s = mul i32 %s,%s%s\n", newcurrentindexmultiplierIdent, currentindexboundIdent, currentindexmultiplierIdent, cgs.debugLocation); err != nil {
				return nil, "", err
			}
			currentindextotalIdent = newcurrentindextotalIdent
			currentindexmultiplierIdent = newcurrentindexmultiplierIdent
		}
	}

	// find pointer to array element
	actualindexIdent := cgs.ident("actual_index")
	arrayelementptrIdent := cgs.ident("array_element_ptr")
	if _, err := fmt.Fprintf(w, "    %s = add i32 %s,%d%s\n", actualindexIdent, currentindextotalIdent, len(arrayElement.Index), cgs.debugLocation); err != nil {
		return nil, "", err
	}
	if _, err := fmt.Fprintf(w, "    %s = getelementptr %%arr_val, %%arr_val* %s, i32 0, i32 3, i32 %s%s\n", arrayelementptrIdent, valptrIdent, actualindexIdent, cgs.debugLocation); err != nil {
		return nil, "", err
	}
	return indexIdentSrcLabels, arrayelementptrIdent, nil
}

func (cgs *codeGenState) genMemset(w io.Writer, memPtrIdent, byteValIdent, sizeIdent string) error {
	if cgs.llvmVersion < 7 {
		if _, err := fmt.Fprintf(w, "    call void @llvm.memset.p0i8.i32(i8* %s,i8 %s,i32 %s,i32 0,i1 0)%s\n", memPtrIdent, byteValIdent, sizeIdent, cgs.debugLocation); err != nil {
			return err
		}
	} else {
		if _, err := fmt.Fprintf(w, "    call void @llvm.memset.p0i8.i32(i8* %s,i8 %s,i32 %s,i1 0)%s\n", memPtrIdent, byteValIdent, sizeIdent, cgs.debugLocation); err != nil {
			return err
		}
	}
	return nil
}

func (cgs *codeGenState) genPushNextStack(w io.Writer, indexToPush, indexForError int) error {
	stackptrIdent := cgs.ident("stackptr")
	if _, err := fmt.Fprintf(w, "    %s = load i32,i32* @stackptr%s\n", stackptrIdent, cgs.debugLocation); err != nil {
		return err
	}
	stackptrCheckIdent := cgs.ident("stackptr_check")
	if _, err := fmt.Fprintf(w, "    %s = icmp uge i32 %s,79%s\n", stackptrCheckIdent, stackptrIdent, cgs.debugLocation); err != nil {
		return err
	}
	stackOverflowLabel := cgs.label("stack_overflow")
	stackOkLabel := cgs.label("stack_ok")
	if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n", stackptrCheckIdent, stackOverflowLabel, stackOkLabel, cgs.debugLocation); err != nil {
		return err
	}
	if _, err := fmt.Fprintf(w, "  %s:\n    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 123,1),i32 %d) noreturn%s\n    ret void%s\n", stackOverflowLabel, indexForError, cgs.debugLocation, cgs.debugLocation); err != nil {
		return err
	}
	if _, err := fmt.Fprintf(w, "  %s:\n", stackOkLabel); err != nil {
		return err
	}
	nextStackptrIdent := cgs.ident("next_stackptr")
	if _, err := fmt.Fprintf(w, "    %s = add i32 %s,1%s\n", nextStackptrIdent, stackptrIdent, cgs.debugLocation); err != nil {
		return err
	}
	if _, err := fmt.Fprintf(w, "    store i32 %s, i32* @stackptr%s\n", nextStackptrIdent, cgs.debugLocation); err != nil {
		return err
	}
	stackentryptrIdent := cgs.ident("next_stack_entry_ptr")
	if _, err := fmt.Fprintf(w, "    %s = getelementptr [79 x i8*], [79 x i8*]* @stack, i32 0, i32 %s%s\n", stackentryptrIdent, stackptrIdent, cgs.debugLocation); err != nil {
		return err
	}
	if _, err := fmt.Fprintf(w, "    store i8* blockaddress(@main,%%stmt%d), i8** %s%s\n", indexToPush, stackentryptrIdent, cgs.debugLocation); err != nil {
		return err
	}
	return nil
}

func (cgs *codeGenState) genGetRand1000(w io.Writer) (string, error) {
	getRandDoneLabel := cgs.label("get_rand_done")
	randIdent := cgs.ident("rand")
	if _, err := fmt.Fprintf(w, "    %s = call i32 @random()%s\n", randIdent, cgs.debugLocation); err != nil {
		return "", err
	}
	phis := [][2]string{}
	tryLabel := cgs.label("try_rand")
	if _, err := fmt.Fprintf(w, "    br label %%%s%s\n", tryLabel, cgs.debugLocation); err != nil {
		return "", err
	}
	for i := 0; i < 20; i++ {
		if _, err := fmt.Fprintf(w, "  %s:\n", tryLabel); err != nil {
			return "", err
		}
		randShiftIdent := cgs.ident("rand_shift")
		if _, err := fmt.Fprintf(w, "    %s = lshr i32 %s,%d%s\n", randShiftIdent, randIdent, i, cgs.debugLocation); err != nil {
			return "", err
		}
		randShiftMaskIdent := cgs.ident("rand_shift_mask")
		if _, err := fmt.Fprintf(w, "    %s = and i32 %s,1023%s\n", randShiftMaskIdent, randShiftIdent, cgs.debugLocation); err != nil {
			return "", err
		}
		phis = append(phis, [2]string{randShiftMaskIdent, tryLabel})
		randShiftMaskOkIdent := cgs.ident("rand_shift_mask_ok")
		if _, err := fmt.Fprintf(w, "    %s = icmp ult i32 %s,1000%s\n", randShiftMaskOkIdent, randShiftMaskIdent, cgs.debugLocation); err != nil {
			return "", err
		}
		tryLabel = cgs.label("try_rand")
		if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n", randShiftMaskOkIdent, getRandDoneLabel, tryLabel, cgs.debugLocation); err != nil {
			return "", err
		}
	}
	if _, err := fmt.Fprintf(w, "  %s:\n", tryLabel); err != nil {
		return "", err
	}
	randFailIdent := cgs.ident("rand_fail")
	phis = append(phis, [2]string{randFailIdent, tryLabel})
	if _, err := fmt.Fprintf(w, "    %s = and i32 %s,511%s\n", randFailIdent, randIdent, cgs.debugLocation); err != nil {
		return "", err
	}
	if _, err := fmt.Fprintf(w, "    br label %%%s%s\n", getRandDoneLabel, cgs.debugLocation); err != nil {
		return "", err
	}

	randResultIdent := cgs.ident("rand_result")
	if _, err := fmt.Fprintf(w, "  %s:\n    %s = phi i32 ", getRandDoneLabel, randResultIdent); err != nil {
		return "", err
	}
	for i, phi := range phis {
		if i > 0 {
			if _, err := fmt.Fprintf(w, ","); err != nil {
				return "", err
			}
		}
		if _, err := fmt.Fprintf(w, "[%s,%%%s]", phi[0], phi[1]); err != nil {
			return "", err
		}
	}
	if _, err := fmt.Fprintf(w, "%s\n", cgs.debugLocation); err != nil {
		return "", err
	}
	return randResultIdent, nil
}

func (cgs *codeGenState) genGotoNext(w io.Writer, nextIndex int, countIdent string) error {
	gotoChance := uint16(0)
	for _, index := range cgs.stmt.Goto {
		if cgs.statements[index].Chance == 0 || (cgs.statements[index].Not && !cgs.stmtInfo[index].reinstated) {
			continue
		}
		gotoChance += cgs.statements[index].Chance
		if gotoChance > 100 {
			break
		}
	}
	if gotoChance > 100 {
		fatalErr555Label := cgs.label("fatal_err_555_")
		comeFromChancesIdent := cgs.ident("come_from_chances")
		if _, err := fmt.Fprintf(w, "    %s = select i1 1,i32 0,i32 0%s\n", comeFromChancesIdent, cgs.debugLocation); err != nil {
			return nil
		}

		gotoChance = 0
		for _, index := range cgs.stmt.Goto {
			if cgs.statements[index].Chance == 0 || (cgs.statements[index].Not && !cgs.stmtInfo[index].reinstated) {
				continue
			}
			if gotoChance <= 100 {
				gotoChance += cgs.statements[index].Chance
			}
			if !cgs.statements[index].Not && !cgs.stmtInfo[index].abstained {
				nextComeFromChancesIdent := cgs.ident("come_from_chances")
				if _, err := fmt.Fprintf(w, "    %s = add i32 %s,%d%s\n", nextComeFromChancesIdent, comeFromChancesIdent, cgs.statements[index].Chance*10, cgs.debugLocation); err != nil {
					return err
				}
				comeFromChancesIdent = nextComeFromChancesIdent
			} else {
				abstainPtrIdent := cgs.ident("abstain_ptr")
				if _, err := fmt.Fprintf(w, "    %s = getelementptr [%d x i1],[%d x i1]* @abstain_flags,i32 0,i32 %d%s\n", abstainPtrIdent, len(cgs.statements)+1, len(cgs.statements)+1, index, cgs.debugLocation); err != nil {
					return err
				}
				abstainIdent := cgs.ident("abstain")
				if _, err := fmt.Fprintf(w, "    %s = load i1,i1* %s%s\n", abstainIdent, abstainPtrIdent, cgs.debugLocation); err != nil {
					return err
				}
				comeFromChanceIdent := cgs.ident("come_from_chance")
				if _, err := fmt.Fprintf(w, "    %s = select i1 %s,i32 0,i32 %d%s\n", comeFromChanceIdent, abstainIdent, cgs.statements[index].Chance*10, cgs.debugLocation); err != nil {
					return err
				}
				nextComeFromChancesIdent := cgs.ident("come_from_chances")
				if _, err := fmt.Fprintf(w, "    %s = add i32 %s,%s%s\n", nextComeFromChancesIdent, comeFromChancesIdent, comeFromChanceIdent, cgs.debugLocation); err != nil {
					return err
				}
				comeFromChancesIdent = nextComeFromChancesIdent
			}
			if gotoChance > 100 {
				checkChancesIdent := cgs.ident("check_chances")
				if _, err := fmt.Fprintf(w, "    %s = icmp ule i32 %s,1000%s\n", checkChancesIdent, comeFromChancesIdent, cgs.debugLocation); err != nil {
					return err
				}
				chancesOkLabel := cgs.label("chances_ok")
				if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", checkChancesIdent, chancesOkLabel, fatalErr555Label, cgs.debugLocation, chancesOkLabel); err != nil {
					return err
				}
			}
		}
		comeFromOkLabel := cgs.label("come_from_ok")
		if _, err := fmt.Fprintf(w, "    br label %%%s%s\n", comeFromOkLabel, cgs.debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "  %s:\n    call void @fatal_error(%%val insertvalue(%%val insertvalue(%%val zeroinitializer,i2 2,0),i32 555,1),i32 %d) noreturn%s\n    ret void%s\n", fatalErr555Label, len(cgs.statements), cgs.debugLocation, cgs.debugLocation); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, "  %s:\n", comeFromOkLabel); err != nil {
			return err
		}
	}

	randomComeFromCount := 0
	for _, index := range cgs.stmt.Goto {
		if cgs.statements[index].Chance == 0 || (cgs.statements[index].Not && !cgs.stmtInfo[index].reinstated) {
			continue
		}
		if cgs.statements[index].Chance < 100 {
			randomComeFromCount++
		}
	}
	randIdent := ""
	if randomComeFromCount > 0 {
		rand1000Ident, err := cgs.genGetRand1000(w)
		if err != nil {
			return err
		}
		randIdent = rand1000Ident
	}
	for _, index := range cgs.stmt.Goto {
		if cgs.statements[index].Chance == 0 || (cgs.statements[index].Not && !cgs.stmtInfo[index].reinstated) {
			continue
		}
		dontComeFromLabel := cgs.label("dont_come_from")
		abstainCheckLabel := ""
		if cgs.statements[index].Not || cgs.stmtInfo[index].abstained {
			abstainCheckLabel = cgs.label("abstain_check")
			if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n", abstainCheckLabel, cgs.debugLocation, abstainCheckLabel); err != nil {
				return err
			}
			abstainPtrIdent := cgs.ident("abstain_ptr")
			if _, err := fmt.Fprintf(w, "    %s = getelementptr [%d x i1],[%d x i1]* @abstain_flags,i32 0,i32 %d%s\n", abstainPtrIdent, len(cgs.statements)+1, len(cgs.statements)+1, index, cgs.debugLocation); err != nil {
				return err
			}
			abstainIdent := cgs.ident("abstain")
			if _, err := fmt.Fprintf(w, "    %s = load i1,i1* %s%s\n", abstainIdent, abstainPtrIdent, cgs.debugLocation); err != nil {
				return err
			}
			notAbstainedLabel := cgs.label("come_from_not_abstained")
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", abstainIdent, dontComeFromLabel, notAbstainedLabel, cgs.debugLocation, notAbstainedLabel); err != nil {
				return err
			}
		}
		newRandIdent := ""
		randDontComeFromLabel := ""
		if cgs.statements[index].Chance < 100 {
			randCheckIdent := cgs.ident("come_from_rand_check")
			if _, err := fmt.Fprintf(w, "    %s = icmp ult i32 %s,%d%s\n", randCheckIdent, randIdent, cgs.statements[index].Chance*10, cgs.debugLocation); err != nil {
				return err
			}
			randDoComeFromLabel := cgs.label("rand_do_come_from")
			randDontComeFromLabel = cgs.label("rand_dont_come_from")
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n", randCheckIdent, randDoComeFromLabel, randDontComeFromLabel, cgs.debugLocation); err != nil {
				return err
			}
			newRandIdent = cgs.ident("come_from_rand")
			if _, err := fmt.Fprintf(w, "  %s:\n    %s = sub i32 %s,%d%s\n", randDontComeFromLabel, newRandIdent, randIdent, cgs.statements[index].Chance*10, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    br label %%%s%s\n", dontComeFromLabel, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "  %s:\n", randDoComeFromLabel); err != nil {
				return err
			}
		}

		if cgs.statements[index].Type == StatementNextFromLabel || cgs.statements[index].Type == StatementNextFromGerundList {
			if err := cgs.genPushNextStack(w, cgs.stmt.Index+1, nextIndex); err != nil {
				return err
			}
		}

		if cgs.stmt.Type != StatementNext {
			if _, err := fmt.Fprintf(w, "    br label %%stmt%d%s\n", index, cgs.debugLocation); err != nil {
				return err
			}
		} else {
			if err := cgs.genPushNextStack(w, index, nextIndex); err != nil {
				return err
			}
			if countIdent != "" && cgs.stmt.Chance > 100 {
				enterLoopLabel := cgs.label("enter_loop")
				if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n", enterLoopLabel, cgs.debugLocation, enterLoopLabel); err != nil {
					return err
				}
				loopLabel := cgs.label("loop")
				if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n", loopLabel, cgs.debugLocation, loopLabel); err != nil {
					return err
				}
				loopCounterIdent := cgs.ident("loop_counter")
				nextLoopCounterIdent := cgs.ident("next_loop_counter")
				loopEndLabel := cgs.label("loop_end")
				if _, err := fmt.Fprintf(w, "    %s = phi i32 [1,%%%s],[%s,%%%s]%s\n", loopCounterIdent, enterLoopLabel, nextLoopCounterIdent, loopEndLabel, cgs.debugLocation); err != nil {
					return err
				}
				if _, err := fmt.Fprintf(w, "    %s = add i32 %s,1%s\n", nextLoopCounterIdent, loopCounterIdent, cgs.debugLocation); err != nil {
					return err
				}
				loopCounterCheckIdent := cgs.ident("loop_counter_check")
				if _, err := fmt.Fprintf(w, "    %s = icmp uge i32 %s,%s%s\n", loopCounterCheckIdent, loopCounterIdent, countIdent, cgs.debugLocation); err != nil {
					return err
				}
				exitLoopLabel := cgs.label("exit_loop")
				loopBodyLabel := cgs.label("loop_body")
				if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", loopCounterCheckIdent, exitLoopLabel, loopBodyLabel, cgs.debugLocation, loopBodyLabel); err != nil {
					return err
				}
				if err := cgs.genPushNextStack(w, nextIndex, nextIndex); err != nil {
					return err
				}
				if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n    br label %%%s%s\n", loopEndLabel, cgs.debugLocation, loopEndLabel, loopLabel, cgs.debugLocation); err != nil {
					return err
				}
				if _, err := fmt.Fprintf(w, "  %s:\n", exitLoopLabel); err != nil {
					return err
				}
			}
			if _, err := fmt.Fprintf(w, "    br label %%stmt%d%s\n", nextIndex, cgs.debugLocation); err != nil {
				return err
			}
		}

		if _, err := fmt.Fprintf(w, "  %s:\n", dontComeFromLabel); err != nil {
			return err
		}
		if newRandIdent != "" {
			if abstainCheckLabel == "" {
				randIdent = newRandIdent
			} else {
				newNewRandIdent := cgs.ident("new_new_rand")
				if _, err := fmt.Fprintf(w, "    %s = phi i32 [%s,%%%s],[%s,%%%s]%s\n", newNewRandIdent, randIdent, abstainCheckLabel, newRandIdent, randDontComeFromLabel, cgs.debugLocation); err != nil {
					return err
				}
				randIdent = newNewRandIdent
			}
		}
	}

	if cgs.stmt.Type != StatementNext {
		if _, err := fmt.Fprintf(w, "    br label %%stmt%d%s\n", nextIndex, cgs.debugLocation); err != nil {
			return err
		}
	} else {
		if err := cgs.genPushNextStack(w, cgs.stmt.Index+1, nextIndex); err != nil {
			return err
		}
		if countIdent != "" && cgs.stmt.Chance > 100 {
			enterLoopLabel := cgs.label("enter_loop")
			if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n", enterLoopLabel, cgs.debugLocation, enterLoopLabel); err != nil {
				return err
			}
			loopLabel := cgs.label("loop")
			if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n", loopLabel, cgs.debugLocation, loopLabel); err != nil {
				return err
			}
			loopCounterIdent := cgs.ident("loop_counter")
			nextLoopCounterIdent := cgs.ident("next_loop_counter")
			loopEndLabel := cgs.label("loop_end")
			if _, err := fmt.Fprintf(w, "    %s = phi i32 [1,%%%s],[%s,%%%s]%s\n", loopCounterIdent, enterLoopLabel, nextLoopCounterIdent, loopEndLabel, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    %s = add i32 %s,1%s\n", nextLoopCounterIdent, loopCounterIdent, cgs.debugLocation); err != nil {
				return err
			}
			loopCounterCheckIdent := cgs.ident("loop_counter_check")
			if _, err := fmt.Fprintf(w, "    %s = icmp uge i32 %s,%s%s\n", loopCounterCheckIdent, loopCounterIdent, countIdent, cgs.debugLocation); err != nil {
				return err
			}
			exitLoopLabel := cgs.label("exit_loop")
			loopBodyLabel := cgs.label("loop_body")
			if _, err := fmt.Fprintf(w, "    br i1 %s,label %%%s,label %%%s%s\n  %s:\n", loopCounterCheckIdent, exitLoopLabel, loopBodyLabel, cgs.debugLocation, loopBodyLabel); err != nil {
				return err
			}
			if err := cgs.genPushNextStack(w, nextIndex, nextIndex); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "    br label %%%s%s\n  %s:\n    br label %%%s%s\n", loopEndLabel, cgs.debugLocation, loopEndLabel, loopLabel, cgs.debugLocation); err != nil {
				return err
			}
			if _, err := fmt.Fprintf(w, "  %s:\n", exitLoopLabel); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(w, "    br label %%stmt%d%s\n", nextIndex, cgs.debugLocation); err != nil {
			return err
		}
	}
	return nil
}
