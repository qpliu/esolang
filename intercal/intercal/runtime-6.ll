declare void @llvm.memcpy.p0i8.p0i8.i32(i8*, i8*, i32, i32, i1)
declare void @llvm.memset.p0i8.i32(i8*, i8, i32, i32, i1)


define void @stash_var(%vrbl* %var) {
    %storeval_ptr = getelementptr %vrbl,%vrbl* %var,i32 0,i32 1

    %mallocresult = call i8* @malloc(i32 ptrtoint(%vrbl_val* getelementptr(%vrbl_val,%vrbl_val* null,i32 1) to i32))
    call void @llvm.memset.p0i8.i32(i8* %mallocresult,i8 0,i32 ptrtoint(%vrbl_val* getelementptr(%vrbl_val,%vrbl_val* null,i32 1) to i32),i32 0,i1 0)
    %newval = bitcast i8* %mallocresult to %vrbl_val*

    %oldvalptr = getelementptr %vrbl,%vrbl* %var,i32 0,i32 1
    %oldval = load %vrbl_val*,%vrbl_val** %oldvalptr
    %oldval_isnull = icmp eq %vrbl_val* %oldval, null
    br i1 %oldval_isnull,label %stash_uninitialized,label %push_newval

  stash_uninitialized:
    store %vrbl_val* %newval,%vrbl_val** %storeval_ptr
    call void @stash_var(%vrbl* %var)
    ret void

  push_newval:
    %oldi32valptr = getelementptr %vrbl_val,%vrbl_val* %oldval,i32 0,i32 2
    %oldi32val = load i32,i32* %oldi32valptr
    %newvallinkptr = getelementptr %vrbl_val,%vrbl_val* %newval,i32 0,i32 0
    store %vrbl_val* %oldval,%vrbl_val** %newvallinkptr
    %newi32valptr = getelementptr %vrbl_val,%vrbl_val* %newval,i32 0,i32 2
    store i32 %oldi32val,i32* %newi32valptr
    br label %store_newval

  store_newval:
    store %vrbl_val* %newval,%vrbl_val** %storeval_ptr
    ret void
}

define void @stash_arr(%arr_vrbl* %arr) {
    %arr_val_ptr = getelementptr %arr_vrbl,%arr_vrbl* %arr,i32 0,i32 1
    %old_arr_val = load %arr_val*,%arr_val** %arr_val_ptr
    %uninitialized = icmp eq %arr_val* %old_arr_val,null
    br i1 %uninitialized,label %stash_uninitialized,label %push_newval

  stash_uninitialized:
    ; push 0-dim value to distinguish from unstashed
    %zerodim_mallocresult = call i8* @malloc(i32 ptrtoint(i32* getelementptr(%arr_val,%arr_val* null,i32 0,i32 3,i32 0) to i32))
    call void @llvm.memset.p0i8.i32(i8* %zerodim_mallocresult,i8 0,i32 ptrtoint(i32* getelementptr(%arr_val,%arr_val* null,i32 0,i32 3,i32 0) to i32),i32 0,i1 0)
    %zerodim_arr_val = bitcast i8* %zerodim_mallocresult to %arr_val*
    store %arr_val* %zerodim_arr_val,%arr_val** %arr_val_ptr
    call void @stash_arr(%arr_vrbl* %arr)
    ret void

  push_newval:
    %olddims_ptr = getelementptr %arr_val,%arr_val* %old_arr_val,i32 0,i32 2
    %olddims = load i32,i32* %olddims_ptr
    %olddims_iszero = icmp eq i32 %olddims,0
    br i1 %olddims_iszero,label %malloc_newval,label %calc_newval_size

  calc_newval_size:
    %current_dim_index = phi i32 [0,%push_newval],[%next_dim_index,%calc_next_newval_size]
    %current_size = phi i32 [0,%push_newval],[%next_size,%calc_next_newval_size]
    %current_mallocsize_index = add i32 %current_size,%olddims
    %current_dims_done = icmp uge i32 %current_dim_index,%olddims
    br i1 %current_dims_done,label %malloc_newval,label %calc_next_newval_size

  calc_next_newval_size:
    %next_dim_index = add i32 %current_dim_index,1
    %current_dim_ptr = getelementptr %arr_val,%arr_val* %old_arr_val,i32 0,i32 3,i32 %current_dim_index
    %current_dim = load i32,i32* %current_dim_ptr
    %next_size = mul i32 %current_dim,%current_size
    br label %calc_newval_size

  malloc_newval:
    %mallocsize_index = phi i32 [0,%push_newval],[%current_mallocsize_index,%calc_newval_size]
    %mallocsize_addr = getelementptr %arr_val,%arr_val* null,i32 0, i32 3,i32 %mallocsize_index
    %mallocsize = ptrtoint i32* %mallocsize_addr to i32
    %mallocresult = call i8* @malloc(i32 %mallocsize)
    %oldval_i8star = bitcast %arr_val* %old_arr_val to i8*
    call void @llvm.memcpy.p0i8.p0i8.i32(i8* %mallocresult,i8* %oldval_i8star,i32 %mallocsize,i32 0,i1 0)
    %new_arr_val = bitcast i8* %mallocresult to %arr_val*
    %new_arr_val_stash_ptr = getelementptr %arr_val,%arr_val* %new_arr_val,i32 0,i32 0
    store %arr_val* %old_arr_val,%arr_val** %new_arr_val_stash_ptr
    store %arr_val* %new_arr_val,%arr_val** %arr_val_ptr
    ret void
}
