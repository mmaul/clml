README for Package CL-STORE.
Author: Sean Ross 
Homepage: http://www.common-lisp.net/project/cl-store/
Version: 0.6

0. About.
   CL-STORE is an portable serialization package which 
   should give you the ability to store all common-lisp
   data types (well not all yet) into streams.
   See the cl-store manual (docs/cl-store.texi) for more in depth information.
   
   !!! NOTE: The cl-store-xml backend is deprecated.

1. Usage
   The main entry points are 
    - [Method] cl-store:store (obj place &optional (backend *default-backend*)) 
          => obj
       Where place is a path designator or stream and
       backend is one of the registered backends.

    - [Method] cl-store:restore (place &optional (backend *default-backend*)) 
          => restored-objects
       Where place and backend is as above.

    - cl-store:restore is setfable, which I think makes
      for a great serialized hit counter.
      eg. (incf (restore place))
  
    NOTE.
     All errors signalled within store and restore can 
     be handled by catching store-error and restore-error respectively.

2. Optimizing.
    
   While cl-store is generally quickish it still has a tendency to 
   do a lot of consing. Thanks to profilers this has been pinned down 
   to the rehashing of the hash-tables which track object circularities.
   From 0.4.0 cl-store has three new variables *store-hash-size*, *restore-hash-size*
   and *check-for-circs*, proper usage of these new variables can greatly reduce
   the consing (and time taken) when storing and restoring large objects.
  
   - *store-hash-size* and *restore-hash-size
     At the beginning of storing and restoring an eq hash-table is created with a 
     default size of 50 to track objects which have been (re)stored. On large objects however
     the rehashing of these hash-tables imposes a severe drain on performance.
     By binding these two variables to appropriately large values 
     about (100010 for a hash-table with 100000 int->string mappings) you
     can obtain a decent performance improvement. This may require a bit 
     of fiddling to find the best tradeoff between rehashing and creating 
     a large hash-table.
 
   - *check-for-circs*
     Binding this variable to nil when storing or restoring
     an object inhibits all checks for circularities which gives a 
     severe boost to performance. The downside of this is that no 
     restored objects will be eq and attempting to store circular objects 
     will hang. The speed improvements are definitely worth it if you 
     know that there will be no circularities or shared references in 
     your data (eg spam-filter hash-tables). 

Enjoy
 Sean.
