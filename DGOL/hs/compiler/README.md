Notes on compiling DGOL
=======================
Call Frame
----------
The call frame data structure is
```c
struct call_frame {
    struct call_frame *caller_call_frame;
    int local_variable_count;
    struct node **local_variable_array;
    int do_edges_iterator_count;
    struct do_edges_iterator *do_edges_iterator_array;
    int call_args_count;
    struct node ***call_args_array;
}
```
`local_variable_count`, the number of local variables, includes all the
subroutine parameters, since the caller could pass in any number of
arguments.  If a caller does not pass in a variable to a parameter, it
will be stored in the `local_variable_array`.

`local_variable_count`, `do_edges_iterator_count`, the number of DO EDGES
statements in the routine, and `call_args_count`, the maximum number of
arguments of a CALL statement in the routine are all determined at
compile time.  `local_variable_count`, `do_edges_iterator_count`, their
corresponding array pointers, as well as `caller_call_frame`, are stored in
the `call_frame` data structure for the mark phase of the garbage collector.
`call_args_count` is stored in the `call_frame` data structure for
callees of this subroutine to access the call arguments.

The `call_frame` data structure and the three arrays it points to are
allocated on the stack when entering the subroutine.

A `do_edges_iterator` data structure may also include a pointer to a heap
allocated array of pointers to `node` data structures, which is initially
null.  When returning from the routine, all arrays pointed to in the
`do_edges_iterator` data structures are freed.

The subroutine arguments are accessed through the `call_args_array` in the
caller call frame.  If the argument index is not less than `call_args_count`
in the caller call frame, or if the corresponding element in
`call_args_array` is the caller call frame is null, then a pointer to the
corresponding element in the `local_variable_array` in the current
call frame is used.

When accessing a variable, if it is null, a new node is obtained from the
allocator and stored in the variable.

When making a call, `call_args_array` is first cleared, then pointers to
the variables being passed as arguments are stored in `call_args_array`.
If an argument is 0, a null pointer is stored.

Node
----
The node data structure is
```c
struct node {
    char gc_mark;
    bool live;
    int edge_array_count;
    struct node **edge_array;
}
```
`gc_mark` and `live` are used by the garbage collector.

`edge_array_count` is initially 0 and `edge_array` is initially null.
`edge_array_count` is the number of elements allocated in `edge_array`, not
the number of edges the node has.  Elements in `edge_array` can be null.

When adding an edge to the node, it is stored in a null element in
`edge_array`.  If there are no null elements, `edge_array_count` is h
increased by 16, `edge_array` is set to a newly allocated array with the
new size with the elements of the original array copied into it and null
stored in the additional elements.  The original array is then freed.  The
edge can then be added to the new array.

When removing an edge from the node, its entry in `edge_array` is
simply set to null.

When the node is garbage collected, `edge_array` is cleared, but retained
and not freed, and `edge_array_count` is retained, to be available when
the node is recycled by the allocator.

Do Edges Iterator
-----------------
The do edges iterator data structure is
```c
struct do_edges_iterator {
    int edge_array_count;
    struct node **edge_array;
}
```
On entering a DO edges loop, the corresponding element in
`do_edges_iterator_array` in the `call_frame` is looked up and
its `edge_array_count` is compared to the `edge_array_count` of
the node whose edges will be iterated over.  If it is greater
than or equal, then the node's `edge_array` is copied into the
iterator's `edge_array`, with any additional element in the
iterator's `edge_array` cleared to null.  Otherwise, the iterators
`edge_array` is freed and replaced with a newly allocated array
with the node's `edge_array_count` elements, and the iterator's
`edge_array_count` set to the number elements in the of the newly
allocated array, and the node's `edge_array` copied into the newly
allocated array.

Then, iterate over each element of `edge_array` in the iterator.
If the element is null, continue to the next iteration.  Otherwise,
set the DO loop variable to the element and execute the statements
in the DO loop body.

Allocator and Garbage Collector
-------------------------------
The page data structure is
```c
struct page {
    struct page *next_page;
    struct node nodes[256];
}
```
The allocator and garbage collector state data structure is
```c
static struct {
    char gc_mark;
    struct page root_page;
} allocator_and_garbage_collector_state;
```
When allocating a new node, iterate over all the nodes in all the pages
until one where `live` is false is found.  Set `live` for that node to
true and return it.

If no such node is found, run the mark and sweep garbage collector.

For the mark phase of garbage collection, increment `gc_mark` in
`allocator_and_garbage_collector_state`, then iterate over all the
nodes in all the local variables and all the edge array iterators in
all the call frames and mark those nodes and all the nodes in their
reachable subgraph with `gc_mark`.

For the sweep phase of garbage collection, iterate over all the
nodes in all the pages and set `live` to false for all nodes for
which `live` is true and `gc_mark` is not equal to `gc_mark` in
`allocator_and_garbage_collector_state`.

Finally, if, after the sweep phase, there are less than 16 (of 256)
nodes in the last page for which `live` is false, allocate a new
page and add it to the end of the page list.

Thoughts on a compacting collector
----------------------------------
Currently, the garbage collector runs when obtaining a new node.
If compacting is done as part of that, then nodes can be moved, so
references to nodes that were loaded before the call to obtain the
new node can become invalid.

For example, when executing
```
  LET A > B
```
either `A` and `B` could be uninitialized, so loading them could
cause allocator to run, which could cause the garbage collector to
run.  If the one loaded second causes the first to be moved due
to compacting, the first has to be reloaded.

So instead of compacting after garbage collection, set a new flag
in `allocator_and_garbage_collector_state` if compaction is
warranted.  Then, check the flag at a safe point, say, when returning
from a subroutine, and run the compactor if the flag is set.

### Compactor algorithm
If there are at least 2 pages with less than 16 live nodes, move all
the nodes from the second page (which will never be the root page) into
the first page, then remove the second page from the linked list of
pages, then free the second page.

### Moving a node
When moving a node, iterate over all the edges of all the live nodes
in all the pages and move the matching edges.  And iterate over all
the local variables in all the call frames and move the matching nodes.
And iterate over all the edges in all the do edge iterators in all the
call frames and move the matching edges.
