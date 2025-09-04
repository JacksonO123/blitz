# Todo

- [ ] implement dereference set value statement
- [ ] implement struct descriptors like copy and any other necessary ones
  - copy will mark if a struct containing a pointer is allowed to be copied
- [ ] add register spilling
- [ ] improve the way functions are stored + used. map function name to number
      and use number to index into array of functions instead of hashmap
- [ ] probably use linked list instead of array list for scope util
- [ ] reduce allocations in scanning
  - replace void type allocations with null
