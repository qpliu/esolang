package main

import (
	"math/rand"
)

func Interp(prog map[string]*Routine, routine *Routine, self *Thread, args []*Thread) {
	vars := map[string]*Thread{"self": self}
	for i, param := range routine.Params {
		if i >= len(args) {
			break
		}
		vars[param.T] = args[i]
	}
	(&interpState{
		prog: prog,
		self: self,
		vars: vars,
		frame: &frame{
			parent: nil,
			loopID: routine.Name.T,
			stmt:   routine.Stmt,
			stmts:  routine.Stmt.Stmts,
			index:  0,
		},
	}).run()
	self.Exit()
}

type interpState struct {
	prog  map[string]*Routine
	self  *Thread
	vars  map[string]*Thread
	frame *frame
}

type frame struct {
	parent *frame

	loopID string
	stmt   *Stmt

	stmts []*Stmt
	index int
}

func (state *interpState) run() {
loop:
	for {
		if state.frame.index >= len(state.frame.stmts) {
			switch state.frame.stmt.Type {
			case StmtLoop:
				state.frame.index = 0
				continue loop
			case StmtMessage:
				state.frame.index = 0
				state.message()
			default:
				panic("stmt.Type")
			}
		}
		if state.frame.index >= len(state.frame.stmts) {
			// empty loop or after message statement with no active arms that was last in the body
			continue
		}

		stmt := state.frame.stmts[state.frame.index]

		threadStates := map[*Thread]bool{}
		state.collectGuardThreadStates(stmt.Guards, threadStates)
		if !state.passGuards(stmt.Guards, threadStates) {
			state.frame.index++
			continue loop
		}

		switch stmt.Type {
		case StmtAssign:
			state.vars[stmt.Vars[0].T] = state.vars[stmt.Exprs[0].T]
			state.frame.index++
		case StmtBreak:
			if len(stmt.LoopID) != 0 {
				for state.frame.loopID != stmt.LoopID[0].T {
					state.frame = state.frame.parent
				}
			}
			state.frame = state.frame.parent
			if state.frame == nil {
				return
			}
			state.frame.index++
		case StmtContinue:
			if len(stmt.LoopID) != 0 {
				for state.frame.loopID != stmt.LoopID[0].T {
					state.frame = state.frame.parent
				}
			}
			state.frame.index = 0
		case StmtLoop:
			state.frame = &frame{
				parent: state.frame,
				stmt:   stmt,
				stmts:  stmt.Stmts,
			}
			if len(stmt.LoopID) > 0 {
				state.frame.loopID = stmt.LoopID[0].T
			}
		case StmtMessage:
			state.frame = &frame{
				parent: state.frame,
				stmt:   stmt,
			}
			if len(stmt.LoopID) > 0 {
				state.frame.loopID = stmt.LoopID[0].T
			}
		case StmtSpawn:
			t := NewThread()
			state.vars[stmt.Vars[0].T] = t
			args := []*Thread{}
			for _, expr := range stmt.Exprs[1:] {
				args = append(args, state.vars[expr.T])
			}
			go Interp(state.prog, state.prog[stmt.Exprs[0].T], t, args)
			state.frame.index++
		default:
			panic("stmt.Type")
		}
	}
}

func (state *interpState) collectGuardThreadStates(guards []Guard, threadStates map[*Thread]bool) {
	for _, guard := range guards {
		if len(guard.Exprs) == 1 {
			expr := state.vars[guard.Exprs[0].T]
			if _, ok := threadStates[expr]; !ok {
				threadStates[expr] = expr.Notify() != MessageResultExited
			}
		}
	}
}

func (state *interpState) passGuards(guards []Guard, threadStates map[*Thread]bool) bool {
	for _, guard := range guards {
		if len(guard.Exprs) > 1 {
			expr0 := state.vars[guard.Exprs[0].T]
			expr1 := state.vars[guard.Exprs[1].T]
			if (expr0 == expr1) != guard.Equal {
				return false
			}
		} else {
			expr := state.vars[guard.Exprs[0].T]
			if threadStates[expr] != guard.Equal {
				return false
			}
		}
	}
	return true
}

func (state *interpState) message() {
	arms := []*Arm{}
	threadStates := map[*Thread]bool{}
	for _, arm := range state.frame.stmt.Arms {
		state.collectGuardThreadStates(arm.Guards, threadStates)
	}
	for _, arm := range state.frame.stmt.Arms {
		if state.passGuards(arm.Guards, threadStates) {
			arms = append(arms, arm)
		}
	}
	if len(arms) == 0 {
		state.frame = state.frame.parent
		state.frame.index++
		return
	}
	rand.Shuffle(len(arms), func(i, j int) {
		arms[i], arms[j] = arms[j], arms[i]
	})

	for {
		result := MessageResultExited
		for _, arm := range arms {
			switch arm.Type {
			case ArmRecv:
				senders := []*Thread{}
				for _, expr := range arm.Exprs {
					senders = append(senders, state.vars[expr.T])
				}
				msg, res := state.self.Dequeue(senders)
				switch res {
				case MessageResultExited:
				case MessageResultBlocked:
					result = MessageResultBlocked
				case MessageResultSuccess:
					state.vars[arm.Vars[0].T] = msg[0]
					state.vars[arm.Vars[1].T] = msg[1]
					state.frame.stmts = arm.Stmts
					state.frame.index = 0
					return
				}
			case ArmSend:
				rcpt := state.vars[arm.Exprs[0].T]
				msg := state.vars[arm.Exprs[1].T]
				switch rcpt.Send(msg, state.self) {
				case MessageResultExited:
				case MessageResultBlocked:
					result = MessageResultBlocked
				case MessageResultSuccess:
					state.frame.stmts = arm.Stmts
					state.frame.index = 0
					return
				}
			default:
				panic("arm.Type")
			}
		}
		if result == MessageResultExited {
			state.frame = state.frame.parent
			state.frame.index++
			return
		}
		state.self.Wait()
	}
}
