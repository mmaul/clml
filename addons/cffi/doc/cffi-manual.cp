\entry{tutorial, }{4}{tutorial, \cffi {}}
\entry{foreign functions and data}{4}{foreign functions and data}
\entry{SWIG}{4}{\acronym {SWIG}}
\entry{Perl}{4}{Perl}
\entry{Python}{4}{Python}
\entry{SLIME}{4}{\acronym {SLIME}}
\entry{advantages of FFI}{4}{advantages of \acronym {FFI}}
\entry{benefits of FFI}{4}{benefits of \acronym {FFI}}
\entry{minimal bindings}{4}{minimal bindings}
\entry{C abstractions}{4}{C abstractions}
\entry{abstractions in C}{4}{abstractions in C}
\entry{workaround for C}{5}{workaround for C}
\entry{cURL}{5}{c\acronym {URL}}
\entry{loading }{5}{loading \cffi {}}
\entry{requiring }{5}{requiring \cffi {}}
\entry{foreign library load}{6}{foreign library load}
\entry{library, foreign}{6}{library, foreign}
\entry{function definition}{6}{function definition}
\entry{calling foreign functions}{6}{calling foreign functions}
\entry{looks like it worked}{7}{looks like it worked}
\entry{pointers in Lisp}{7}{pointers in Lisp}
\entry{varargs}{7}{varargs}
\entry{foreign arguments}{7}{foreign arguments}
\entry{enumeration, C}{7}{enumeration, C}
\entry{breaking the abstraction}{9}{breaking the abstraction}
\entry{abstraction breaking}{9}{abstraction breaking}
\entry{streams and C}{9}{streams and C}
\entry{file* and streams}{9}{\sc {file}* and streams}
\entry{Lispy C functions}{10}{Lispy C functions}
\entry{dynamic extent}{12}{dynamic extent}
\entry{foreign values with dynamic extent}{12}{foreign values with dynamic extent}
\entry{premature deallocation}{12}{premature deallocation}
\entry{strings}{14}{strings}
\entry{callback definition}{16}{callback definition}
\entry{defining callbacks}{16}{defining callbacks}
\entry{type definition}{18}{type definition}
\entry{data in Lisp and C}{18}{data in Lisp and C}
\entry{translating types}{18}{translating types}
\entry{limitations of type translators}{20}{limitations of type translators}
\entry{:char}{22}{\code {:char}}
\entry{:unsigned-char}{22}{\code {:unsigned-char}}
\entry{:short}{22}{\code {:short}}
\entry{:unsigned-short}{22}{\code {:unsigned-short}}
\entry{:int}{22}{\code {:int}}
\entry{:unsigned-int}{22}{\code {:unsigned-int}}
\entry{:long}{22}{\code {:long}}
\entry{:unsigned-long}{22}{\code {:unsigned-long}}
\entry{:long-long}{22}{\code {:long-long}}
\entry{:unsigned-long-long}{22}{\code {:unsigned-long-long}}
\entry{:uchar}{22}{\code {:uchar}}
\entry{:ushort}{22}{\code {:ushort}}
\entry{:uint}{22}{\code {:uint}}
\entry{:ulong}{22}{\code {:ulong}}
\entry{:llong}{22}{\code {:llong}}
\entry{:ullong}{22}{\code {:ullong}}
\entry{:int8}{22}{\code {:int8}}
\entry{:uint8}{22}{\code {:uint8}}
\entry{:int16}{22}{\code {:int16}}
\entry{:uint16}{22}{\code {:uint16}}
\entry{:int32}{22}{\code {:int32}}
\entry{:uint32}{22}{\code {:uint32}}
\entry{:int64}{23}{\code {\xeatspaces {:int64}}}
\entry{:uint64}{23}{\code {\xeatspaces {:uint64}}}
\entry{:float}{23}{\code {\xeatspaces {:float}}}
\entry{:double}{23}{\code {\xeatspaces {:double}}}
\entry{:long-double}{23}{\code {\xeatspaces {:long-double}}}
\entry{:pointer &optional type}{23}{\code {\xeatspaces {:pointer &optional type}}}
\entry{:void}{23}{\code {\xeatspaces {:void}}}
\entry{:string}{23}{\code {\xeatspaces {:string}}}
\entry{:string+ptr}{23}{\code {\xeatspaces {:string+ptr}}}
\entry{:boolean &optional (base-type :int)}{24}{\code {\xeatspaces {:boolean &optional (base-type :int)}}}
\entry{:wrapper base-type &key to-c from-c}{24}{\code {\xeatspaces {:wrapper base-type &key to-c from-c}}}
\entry{type translators, optimizing}{26}{type translators, optimizing}
\entry{compiler macros for type translation}{26}{compiler macros for type translation}
\entry{defining type-translation compiler macros}{26}{defining type-translation compiler macros}
\entry{convert-from-foreign foreign-value type  value}{29}{\code {\xeatspaces {convert-from-foreign foreign-value type \res {} value}}}
\entry{convert-to-foreign value type  foreign-value, alloc-params}{30}{\code {\xeatspaces {convert-to-foreign value type \res {} foreign-value, alloc-params}}}
\entry{defbitfield name-and-options &body masks}{31}{\code {\xeatspaces {defbitfield name-and-options &body masks}}}
\entry{defcstruct name-and-options &body doc-and-slots  name}{33}{\code {\xeatspaces {defcstruct name-and-options &body doc-and-slots \res {} name}}}
\entry{defcunion name &body doc-and-slots  name}{35}{\code {\xeatspaces {defcunion name &body doc-and-slots \res {} name}}}
\entry{defctype name base-type &optional documentation}{36}{\code {\xeatspaces {defctype name base-type &optional documentation}}}
\entry{defcenum name-and-options &body enum-list}{37}{\code {\xeatspaces {defcenum name-and-options &body enum-list}}}
\entry{define-foreign-type class-name supers slots &rest options  class-name}{38}{\code {\xeatspaces {define-foreign-type class-name supers slots &rest options \res {} class-name}}}
\entry{define-parse-method name lambda-list &body body  name}{39}{\code {\xeatspaces {define-parse-method name lambda-list &body body \res {} name}}}
\entry{foreign-bitfield-symbols type value  symbols}{40}{\code {\xeatspaces {foreign-bitfield-symbols type value \res {} symbols}}}
\entry{foreign-bitfield-value type symbols  value}{41}{\code {\xeatspaces {foreign-bitfield-value type symbols \res {} value}}}
\entry{foreign-enum-keyword type value &key errorp  keyword}{42}{\code {\xeatspaces {foreign-enum-keyword type value &key errorp \res {} keyword}}}
\entry{foreign-enum-value type keyword &key errorp  value}{43}{\code {\xeatspaces {foreign-enum-value type keyword &key errorp \res {} value}}}
\entry{foreign-slot-names type  names}{44}{\code {\xeatspaces {foreign-slot-names type \res {} names}}}
\entry{foreign-slot-offset type slot-name  offset}{45}{\code {\xeatspaces {foreign-slot-offset type slot-name \res {} offset}}}
\entry{foreign-slot-pointer ptr type slot-name  pointer}{46}{\code {\xeatspaces {foreign-slot-pointer ptr type slot-name \res {} pointer}}}
\entry{foreign-slot-value ptr type slot-name  object}{47}{\code {\xeatspaces {foreign-slot-value ptr type slot-name \res {} object}}}
\entry{foreign-type-alignment type  alignment}{48}{\code {\xeatspaces {foreign-type-alignment type \res {} alignment}}}
\entry{foreign-type-size type  size}{49}{\code {\xeatspaces {foreign-type-size type \res {} size}}}
\entry{free-converted-object foreign-value type params}{50}{\code {\xeatspaces {free-converted-object foreign-value type params}}}
\entry{free-translated-object value type-name param}{51}{\code {\xeatspaces {free-translated-object value type-name param}}}
\entry{translate-from-foreign foreign-value type-name   lisp-value}{52}{\code {\xeatspaces {translate-from-foreign foreign-value type-name \ \res {} lisp-value}}}
\entry{translate-to-foreign lisp-value type-name   foreign-value, alloc-param}{53}{\code {\xeatspaces {translate-to-foreign lisp-value type-name \ \res {} foreign-value, alloc-param}}}
\entry{with-foreign-slots (vars ptr type) &body body}{54}{\code {\xeatspaces {with-foreign-slots (vars ptr type) &body body}}}
\entry{foreign-pointer}{55}{\code {foreign-pointer}}
\entry{foreign-free ptr  undefined}{57}{\code {\xeatspaces {foreign-free ptr \res {} undefined}}}
\entry{foreign-alloc type &key initial-element initial-contents (count 1)  null-terminated-p  pointer}{58}{\code {\xeatspaces {foreign-alloc type &key initial-element initial-contents (count 1) \ null-terminated-p \res {} pointer}}}
\entry{foreign-symbol-pointer foreign-name &key library  pointer}{60}{\code {\xeatspaces {foreign-symbol-pointer foreign-name &key library \res {} pointer}}}
\entry{inc-pointer pointer offset  new-pointer}{61}{\code {\xeatspaces {inc-pointer pointer offset \res {} new-pointer}}}
\entry{incf-pointer place &optional (offset 1)  new-pointer}{62}{\code {\xeatspaces {incf-pointer place &optional (offset 1) \res {} new-pointer}}}
\entry{make-pointer address  ptr}{63}{\code {\xeatspaces {make-pointer address \res {} ptr}}}
\entry{mem-aref ptr type &optional (index 0)}{64}{\code {\xeatspaces {mem-aref ptr type &optional (index 0)}}}
\entry{mem-ref ptr type &optional offset  object}{65}{\code {\xeatspaces {mem-ref ptr type &optional offset \res {} object}}}
\entry{null-pointer  pointer}{66}{\code {\xeatspaces {null-pointer \res {} pointer}}}
\entry{null-pointer-p ptr  boolean}{67}{\code {\xeatspaces {null-pointer-p ptr \res {} boolean}}}
\entry{pointerp ptr  boolean}{68}{\code {\xeatspaces {pointerp ptr \res {} boolean}}}
\entry{pointer-address ptr  address}{69}{\code {\xeatspaces {pointer-address ptr \res {} address}}}
\entry{pointer-eq ptr1 ptr2  boolean}{70}{\code {\xeatspaces {pointer-eq ptr1 ptr2 \res {} boolean}}}
\entry{with-foreign-object (var type &optional count) &body body}{71}{\code {\xeatspaces {with-foreign-object (var type &optional count) &body body}}}
\entry{with-foreign-objects (bindings) &body body}{71}{\code {\xeatspaces {with-foreign-objects (bindings) &body body}}}
\entry{with-foreign-pointer (var size &optional size-var) &body body}{72}{\code {\xeatspaces {with-foreign-pointer (var size &optional size-var) &body body}}}
\entry{foreign-string-alloc string &key encoding null-terminated-p  start end  pointer}{75}{\code {\xeatspaces {foreign-string-alloc string &key encoding null-terminated-p \ start end \res {} pointer}}}
\entry{foreign-string-free pointer}{76}{\code {\xeatspaces {foreign-string-free pointer}}}
\entry{foreign-string-to-lisp ptr &optional offset count max-chars  encoding  string}{77}{\code {\xeatspaces {foreign-string-to-lisp ptr &optional offset count max-chars \ encoding \res {} string}}}
\entry{lisp-string-to-foreign string buffer bufsize &key start  end offset encoding  buffer}{78}{\code {\xeatspaces {lisp-string-to-foreign string buffer bufsize &key start \ end offset encoding \res {} buffer}}}
\entry{with-foreign-string (var-or-vars string &rest args) &body body}{79}{\code {\xeatspaces {with-foreign-string (var-or-vars string &rest args) &body body}}}
\entry{with-foreign-strings (bindings) &body body}{79}{\code {\xeatspaces {with-foreign-strings (bindings) &body body}}}
\entry{with-foreign-pointer-as-string (var size &optional size-var  &rest args) &body body  string}{80}{\code {\xeatspaces {with-foreign-pointer-as-string (var size &optional size-var \ &rest args) &body body \res {} string}}}
\entry{defcvar name-and-options type &optional documentation  lisp-name}{82}{\code {\xeatspaces {defcvar name-and-options type &optional documentation \res {} lisp-name}}}
\entry{get-var-pointer symbol  pointer}{84}{\code {\xeatspaces {get-var-pointer symbol \res {} pointer}}}
\entry{defcfun name-and-options return-type &body [docstring] arguments [&rest]   lisp-name}{86}{\code {\xeatspaces {defcfun name-and-options return-type &body [docstring] arguments [&rest] \ \res {} lisp-name}}}
\entry{foreign-funcall name-and-options &rest arguments  return-value}{88}{\code {\xeatspaces {foreign-funcall name-and-options &rest arguments \res {} return-value}}}
\entry{foreign-funcall-pointer pointer options &rest arguments  return-value}{90}{\code {\xeatspaces {foreign-funcall-pointer pointer options &rest arguments \res {} return-value}}}
\entry{close-foreign-library library  success}{92}{\code {\xeatspaces {close-foreign-library library \res {} success}}}
\entry{define-foreign-library name-and-options {\tt \char 123} load-clause {\tt \char 125}*  name}{94}{\code {\xeatspaces {define-foreign-library name-and-options {\tt \char 123} load-clause {\tt \char 125}* \res {} name}}}
\entry{load-foreign-library library  handler}{97}{\code {\xeatspaces {load-foreign-library library \res {} handler}}}
\entry{load-foreign-library-error}{99}{\code {\xeatspaces {load-foreign-library-error}}}
\entry{use-foreign-library name}{100}{\code {\xeatspaces {use-foreign-library name}}}
\entry{callback symbol  pointer}{102}{\code {\xeatspaces {callback symbol \res {} pointer}}}
\entry{defcallback name-and-options return-type arguments &body body  name}{103}{\code {\xeatspaces {defcallback name-and-options return-type arguments &body body \res {} name}}}
\entry{get-callback symbol  pointer}{105}{\code {\xeatspaces {get-callback symbol \res {} pointer}}}
\entry{progn}{106}{\code {progn}}
\entry{include}{106}{\code {include}}
\entry{in-package}{106}{\code {in-package}}
\entry{ctype}{106}{\code {ctype}}
\entry{constant}{106}{\code {constant}}
\entry{define}{107}{\code {define}}
\entry{cc-flags}{107}{\code {cc-flags}}
\entry{cstruct}{107}{\code {cstruct}}
\entry{cunion}{107}{\code {cunion}}
\entry{cstruct-and-class}{107}{\code {cstruct-and-class}}
\entry{cvar}{107}{\code {cvar}}
\entry{cenum}{107}{\code {cenum}}
\entry{constantenum}{107}{\code {constantenum}}
