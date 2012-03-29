\initial {:}
\entry {\code {\xeatspaces {:boolean &optional (base-type :int)}}}{24}
\entry {\code {:char}}{22}
\entry {\code {\xeatspaces {:double}}}{23}
\entry {\code {\xeatspaces {:float}}}{23}
\entry {\code {:int}}{22}
\entry {\code {:int16}}{22}
\entry {\code {:int32}}{22}
\entry {\code {\xeatspaces {:int64}}}{23}
\entry {\code {:int8}}{22}
\entry {\code {:llong}}{22}
\entry {\code {:long}}{22}
\entry {\code {\xeatspaces {:long-double}}}{23}
\entry {\code {:long-long}}{22}
\entry {\code {\xeatspaces {:pointer &optional type}}}{23}
\entry {\code {:short}}{22}
\entry {\code {\xeatspaces {:string}}}{23}
\entry {\code {\xeatspaces {:string+ptr}}}{23}
\entry {\code {:uchar}}{22}
\entry {\code {:uint}}{22}
\entry {\code {:uint16}}{22}
\entry {\code {:uint32}}{22}
\entry {\code {\xeatspaces {:uint64}}}{23}
\entry {\code {:uint8}}{22}
\entry {\code {:ullong}}{22}
\entry {\code {:ulong}}{22}
\entry {\code {:unsigned-char}}{22}
\entry {\code {:unsigned-int}}{22}
\entry {\code {:unsigned-long}}{22}
\entry {\code {:unsigned-long-long}}{22}
\entry {\code {:unsigned-short}}{22}
\entry {\code {:ushort}}{22}
\entry {\code {\xeatspaces {:void}}}{23}
\entry {\code {\xeatspaces {:wrapper base-type &key to-c from-c}}}{24}
\initial {A}
\entry {abstraction breaking}{9}
\entry {abstractions in C}{4}
\entry {advantages of \acronym {FFI}}{4}
\initial {B}
\entry {benefits of \acronym {FFI}}{4}
\entry {breaking the abstraction}{9}
\initial {C}
\entry {C abstractions}{4}
\entry {callback definition}{16}
\entry {\code {\xeatspaces {callback symbol \res {} pointer}}}{102}
\entry {calling foreign functions}{6}
\entry {\code {cc-flags}}{107}
\entry {\code {cenum}}{107}
\entry {\code {\xeatspaces {close-foreign-library library \res {} success}}}{92}
\entry {compiler macros for type translation}{26}
\entry {\code {constant}}{106}
\entry {\code {constantenum}}{107}
\entry {\code {\xeatspaces {convert-from-foreign foreign-value type \res {} value}}}{29}
\entry {\code {\xeatspaces {convert-to-foreign value type \res {} foreign-value, alloc-params}}}{30}
\entry {\code {cstruct}}{107}
\entry {\code {cstruct-and-class}}{107}
\entry {\code {ctype}}{106}
\entry {\code {cunion}}{107}
\entry {c\acronym {URL}}{5}
\entry {\code {cvar}}{107}
\initial {D}
\entry {data in Lisp and C}{18}
\entry {\code {\xeatspaces {defbitfield name-and-options &body masks}}}{31}
\entry {\code {\xeatspaces {defcallback name-and-options return-type arguments &body body \res {} name}}}{103}
\entry {\code {\xeatspaces {defcenum name-and-options &body enum-list}}}{37}
\entry {\code {\xeatspaces {defcfun name-and-options return-type &body [docstring] arguments [&rest] \ \res {} lisp-name}}}{86}
\entry {\code {\xeatspaces {defcstruct name-and-options &body doc-and-slots \res {} name}}}{33}
\entry {\code {\xeatspaces {defctype name base-type &optional documentation}}}{36}
\entry {\code {\xeatspaces {defcunion name &body doc-and-slots \res {} name}}}{35}
\entry {\code {\xeatspaces {defcvar name-and-options type &optional documentation \res {} lisp-name}}}{82}
\entry {\code {define}}{107}
\entry {\code {\xeatspaces {define-foreign-library name-and-options {\tt \char 123} load-clause {\tt \char 125}* \res {} name}}}{94}
\entry {\code {\xeatspaces {define-foreign-type class-name supers slots &rest options \res {} class-name}}}{38}
\entry {\code {\xeatspaces {define-parse-method name lambda-list &body body \res {} name}}}{39}
\entry {defining callbacks}{16}
\entry {defining type-translation compiler macros}{26}
\entry {dynamic extent}{12}
\initial {E}
\entry {enumeration, C}{7}
\initial {F}
\entry {\sc {file}* and streams}{9}
\entry {foreign arguments}{7}
\entry {foreign functions and data}{4}
\entry {foreign library load}{6}
\entry {foreign values with dynamic extent}{12}
\entry {\code {\xeatspaces {foreign-alloc type &key initial-element initial-contents (count 1) \ null-terminated-p \res {} pointer}}}{58}
\entry {\code {\xeatspaces {foreign-bitfield-symbols type value \res {} symbols}}}{40}
\entry {\code {\xeatspaces {foreign-bitfield-value type symbols \res {} value}}}{41}
\entry {\code {\xeatspaces {foreign-enum-keyword type value &key errorp \res {} keyword}}}{42}
\entry {\code {\xeatspaces {foreign-enum-value type keyword &key errorp \res {} value}}}{43}
\entry {\code {\xeatspaces {foreign-free ptr \res {} undefined}}}{57}
\entry {\code {\xeatspaces {foreign-funcall name-and-options &rest arguments \res {} return-value}}}{88}
\entry {\code {\xeatspaces {foreign-funcall-pointer pointer options &rest arguments \res {} return-value}}}{90}
\entry {\code {foreign-pointer}}{55}
\entry {\code {\xeatspaces {foreign-slot-names type \res {} names}}}{44}
\entry {\code {\xeatspaces {foreign-slot-offset type slot-name \res {} offset}}}{45}
\entry {\code {\xeatspaces {foreign-slot-pointer ptr type slot-name \res {} pointer}}}{46}
\entry {\code {\xeatspaces {foreign-slot-value ptr type slot-name \res {} object}}}{47}
\entry {\code {\xeatspaces {foreign-string-alloc string &key encoding null-terminated-p \ start end \res {} pointer}}}{75}
\entry {\code {\xeatspaces {foreign-string-free pointer}}}{76}
\entry {\code {\xeatspaces {foreign-string-to-lisp ptr &optional offset count max-chars \ encoding \res {} string}}}{77}
\entry {\code {\xeatspaces {foreign-symbol-pointer foreign-name &key library \res {} pointer}}}{60}
\entry {\code {\xeatspaces {foreign-type-alignment type \res {} alignment}}}{48}
\entry {\code {\xeatspaces {foreign-type-size type \res {} size}}}{49}
\entry {\code {\xeatspaces {free-converted-object foreign-value type params}}}{50}
\entry {\code {\xeatspaces {free-translated-object value type-name param}}}{51}
\entry {function definition}{6}
\initial {G}
\entry {\code {\xeatspaces {get-callback symbol \res {} pointer}}}{105}
\entry {\code {\xeatspaces {get-var-pointer symbol \res {} pointer}}}{84}
\initial {I}
\entry {\code {in-package}}{106}
\entry {\code {\xeatspaces {inc-pointer pointer offset \res {} new-pointer}}}{61}
\entry {\code {\xeatspaces {incf-pointer place &optional (offset 1) \res {} new-pointer}}}{62}
\entry {\code {include}}{106}
\initial {L}
\entry {library, foreign}{6}
\entry {limitations of type translators}{20}
\entry {\code {\xeatspaces {lisp-string-to-foreign string buffer bufsize &key start \ end offset encoding \res {} buffer}}}{78}
\entry {Lispy C functions}{10}
\entry {\code {\xeatspaces {load-foreign-library library \res {} handler}}}{97}
\entry {\code {\xeatspaces {load-foreign-library-error}}}{99}
\entry {loading \cffi {}}{5}
\entry {looks like it worked}{7}
\initial {M}
\entry {\code {\xeatspaces {make-pointer address \res {} ptr}}}{63}
\entry {\code {\xeatspaces {mem-aref ptr type &optional (index 0)}}}{64}
\entry {\code {\xeatspaces {mem-ref ptr type &optional offset \res {} object}}}{65}
\entry {minimal bindings}{4}
\initial {N}
\entry {\code {\xeatspaces {null-pointer \res {} pointer}}}{66}
\entry {\code {\xeatspaces {null-pointer-p ptr \res {} boolean}}}{67}
\initial {P}
\entry {Perl}{4}
\entry {\code {\xeatspaces {pointer-address ptr \res {} address}}}{69}
\entry {\code {\xeatspaces {pointer-eq ptr1 ptr2 \res {} boolean}}}{70}
\entry {\code {\xeatspaces {pointerp ptr \res {} boolean}}}{68}
\entry {pointers in Lisp}{7}
\entry {premature deallocation}{12}
\entry {\code {progn}}{106}
\entry {Python}{4}
\initial {R}
\entry {requiring \cffi {}}{5}
\initial {S}
\entry {\acronym {SLIME}}{4}
\entry {streams and C}{9}
\entry {strings}{14}
\entry {\acronym {SWIG}}{4}
\initial {T}
\entry {\code {\xeatspaces {translate-from-foreign foreign-value type-name \ \res {} lisp-value}}}{52}
\entry {\code {\xeatspaces {translate-to-foreign lisp-value type-name \ \res {} foreign-value, alloc-param}}}{53}
\entry {translating types}{18}
\entry {tutorial, \cffi {}}{4}
\entry {type definition}{18}
\entry {type translators, optimizing}{26}
\initial {U}
\entry {\code {\xeatspaces {use-foreign-library name}}}{100}
\initial {V}
\entry {varargs}{7}
\initial {W}
\entry {\code {\xeatspaces {with-foreign-object (var type &optional count) &body body}}}{71}
\entry {\code {\xeatspaces {with-foreign-objects (bindings) &body body}}}{71}
\entry {\code {\xeatspaces {with-foreign-pointer (var size &optional size-var) &body body}}}{72}
\entry {\code {\xeatspaces {with-foreign-pointer-as-string (var size &optional size-var \ &rest args) &body body \res {} string}}}{80}
\entry {\code {\xeatspaces {with-foreign-slots (vars ptr type) &body body}}}{54}
\entry {\code {\xeatspaces {with-foreign-string (var-or-vars string &rest args) &body body}}}{79}
\entry {\code {\xeatspaces {with-foreign-strings (bindings) &body body}}}{79}
\entry {workaround for C}{5}
