# Oberon-type-case-statement-without-loopholes
Type case statement without type loopholes for the Oberon-07 programming language

Note: In this repository, the term "Project Oberon 2013" refers to a re-implementation of the original "Project Oberon" on an FPGA development board around 2013, as published at www.projectoberon.com.

**PREREQUISITES**: A current version of Project Oberon 2013 (see http://www.projectoberon.com). If you use Extended Oberon (see http://github.com/andreaspirklbauer/Oberon-extended), the functionality is already implemented.

------------------------------------------------------

This repository modifies the semantics of the *type* case statement of Oberon-07

    CASE x OF
     | T1: S1
     | T2: S2
     | T3: S3
    END

such that the following rules and restrictions apply:

* The case variable must be a *simple* identifier that cannot be followed by a selector, i.e. it cannot be an element of a structure (array element or record field).

* The case variable must be either a *local* variable or *value* parameter of pointer type (pointing to a record) or a *variable parameter* of record type.

* A case variable of pointer type cannot be assigned a different value or passed as a *variable* parameter to a procedure within the scope of the type case statement. However, individual elements (fields) of a case variable may be modified or passed as variable parameters.

This eliminates a number of type loopholes. See the test program [**TestTypeCase.Mod**](Sources/FPGAOberon2013/TestTypeCase.Mod).

------------------------------------------------------
**Preparing your system to use the modified Oberon compiler**

If *Extended Oberon* is used, the functionality is already implemented on your system.

If *Project Oberon 2013* is used, follow the instructions below:

------------------------------------------------------

Convert the downloaded files to Oberon format (Oberon uses CR as line endings) using the command [**dos2oberon**](dos2oberon), also available in this repository (example shown for Mac or Linux):

     for x in *.Mod ; do ./dos2oberon $x $x ; done

Import the files to your Oberon system. If you use an emulator (e.g., **https://github.com/pdewacht/oberon-risc-emu**) to run the Oberon system, click on the *PCLink1.Run* link in the *System.Tool* viewer, copy the files to the emulator directory, and execute the following command on the command shell of your host system:

     cd oberon-risc-emu
     for x in *.Mod ; do ./pcreceive.sh $x ; sleep 0.5 ; done

Compile the downloaded files as follows:

     ORP.Compile ORG.Mod/s ORP.Mod/s ~  # build the MODIFIED compiler
     System.Free ORP ORG ~              # unload the ORIGINAL compiler

     ORP.Compile TestTypeCase.Mod/s ~   # compile the test program with the MODIFIED compiler

------------------------------------------------------
**Implementation**

First, we implement the two rules

* The case variable must be a *simple* identifier that cannot be followed by a selector, i.e. it cannot be an element of a structure (array element or record field).

* The case variable must be either a *local* variable or *value* parameter of pointer type (pointing to a record) or a *variable parameter* of record type.

The first rule is implemented by simply *not* calling *ORP.selector* after calling *qualident* when the case variable is parsed. The second rule is implemented by adding the check

    IF (obj.lev <= 0) OR
      (obj.type.form = ORB.Pointer) & ((obj.class # ORB.Var) OR (obj.type.base.form # ORB.Record)) OR
      (obj.type.form = ORB.Record) & (obj.class # ORB.Par)

to procedure *ORP.StatSequence*, which now reads:

    PROCEDURE StatSequence;
      ...
      ELSIF sym = ORS.case THEN  (*case statement*)
        ORS.Get(sym);
        IF sym = ORS.ident THEN
          qualident(obj);  (*no call to selector here*)
          IF (obj.type.form IN {ORB.Pointer, ORB.Record}) THEN  (*type case statement*)
            IF (obj.type.form = ORB.Pointer) & ((obj.class # ORB.Var) OR (obj.type.base.form # ORB.Record)) OR
              (obj.type.form = ORB.Record) & (obj.class # ORB.Par) OR (obj.lev <= 0)
            THEN ORS.Mark("invalid case expression")
            END ;
            TypeCasePart(obj)

Second, we implement the rule 

* A case variable of pointer type cannot be assigned a different value or passed as a *variable* parameter to a procedure within the scope of the type case statement. However, individual elements (fields) of a case variable may be modified or passed as variable parameters.

To do so, we need a way to detect whether any object *obj* encountered during compilation is in fact a case variable of a type case statement.

Recall that in the official Oberon-07 compiler, the following invariant holds for all declared objects *obj* during compilation:

    obj.lev > 0  =>  ~obj.expo & (obj.exno = 0)

This simply means that if an object is a *local* variable or a formal *parameter*, it cannot have an export mark (asterisk).

Since the case variable of a *type* case statement must be either a *local* variable or procedure *parameter* (*obj.lev > 0* always), we can "abuse" either the field *obj.expo* or the field *obj.exno* (or both) to indicate that we are *inside* a type case statement.

For example, we may define

    (obj.lev > 0) & (obj.exno > 0)  =>  we are inside a type case statement (and obj is the case variable)

and let the code implementing the type case statement temporarily set *obj.exno* to a value different that 0.

Recall that the *type* case statement represents the singular case where a symbol table entry - the type of *obj* - is temporarily modified during compilation (but only until the end of the statement, where the change is reverted). With the above change, we add another temporary modification to the (same) symbol table entry (namely the field *obj.exno*).

In order to handle *nested* type case statements correctly, we increase the value of *obj.expo* by 1 when entering a type case statement and decrease it by 1 when exiting it, i.e. the current value of the field *obj.exno* effectively represents the nesting level of type case statements.

    PROCEDURE Sample();
      VAR p: P0;       (*obj is the local variable p => obj.lev = 1 & ~obj.expo & obj.exno = 0*)
    BEGIN
      p := p1;         (*correct, since we are outside ANY type case statements*)
      CASE p OF        (*increase obj.exno from 0 to 1*)
        P1:
          p := p2;     (*"read-only" compile-time error*)
          CASE p OF    (*increase obj.exno from 1 to 2*)
          P1:
            p := p1    (*"read-only" compile-time error*)
          END ;        (*decrease obj.exno from 2 to 1 = the value before entering the inner case statement*)
          p := p1      (*"read-only" compile-time error, since we are still inside the outer case statement*)
      END ;            (*decrease obj.exno from 1 to 0 = the value before entering the outer case statement*)
      p := p1          (*correct again, since we are now outside ANY type case statements again*)
    END Sample;

This leads to the following code:

     PROCEDURE TypeCasePart(obj: ORB.Object);
       VAR L0: LONGINT;
     BEGIN Check(ORS.of, "OF expected"); L0 := 0; INC(obj.exno);  (*<---*)
       WHILE (sym < ORS.end) OR (sym = ORS.bar) DO
         IF sym = ORS.bar THEN ORS.Get(sym) ELSE TypeCase(obj, L0) END
       END ;
       IF sym = ORS.else THEN ORS.Get(sym); StatSequence END ;
       ORG.FixLink(L0); DEC(obj.exno)  (*<---*)
     END TypeCasePart;

Since this is the only place where *obj.exno* is modified, it is easy to see that the *only* place where the condition

    (obj.lev > 0) & (obj.exno > 0)
    
can ever be true is *inside* a type case statement.

We must also be able to recognize whether the identified case variable is a *simple* identifier that is *not* followed by a selector. To achieve this, we first extend *ORP.selector* to return this fact as a boolean variable parameter *sel*

    PROCEDURE selector(VAR x: ORG.Item; VAR sel: BOOLEAN);  (*variable parameter 'sel' added*)

and add it to the above condition, leading to:

    ~sel & (obj.lev > 0) & (obj.exno > 0)  =>  we are inside a type case statement (and obj is the case variable)

Finally, we must be able to recognize whether the case variable is of *pointer* type. This adds the condition *obj.type.form = ORP.Pointer* to the above expression. Putting all together, wo obtain:

    PROCEDURE CheckTypeCase(VAR x: ORG.Item; obj: ORB.Object; sel: BOOLEAN);
    BEGIN
      IF ~sel & (obj.lev > 0) & (obj.exno > 0) & (obj.type.form = ORB.Pointer) THEN x.rdo := TRUE END
    END CheckTypeCase;

We now add this check when assignments and procedure parameters are parsed:

*1. Assignments to case variables*

    PROCEDURE StatSequence;
      ...
      IF sym = ORS.ident THEN
        qualident(obj); ORG.MakeItem(x, obj, level);
        IF x.mode = ORB.SProc THEN StandProc(obj.val)
        ELSE selector(x, sel);
          IF sym = ORS.becomes THEN (*assignment*)
            ORS.Get(sym); CheckTypeCase(x, obj, sel); CheckReadOnly(x); expression(y);  (*CheckTypeCase may set x.rdo*)

*2. Passing case variables as procedure parameters*

    PROCEDURE Parameter(par: ORB.Object);  (*par is the formal parameter, x the actual parameter*)
      VAR x: ORG.Item; varpar: BOOLEAN;
    BEGIN expression(x);                               (*calls ORP.factor, which calls CheckTypeCase, which in turn may set x.rdo*)
      IF par # NIL THEN
        varpar := par.class = ORB.Par;
        IF CompTypes(par.type, x.type, varpar) THEN
          IF ~varpar THEN ORG.ValueParam(x)
          ELSE (*par.class = Par, i.e VAR parameter*)
            IF ~par.rdo THEN CheckReadOnly(x) END ;    (*issues an error message if CheckTypeCase has set x.rdo*)
            ORG.VarParam(x, par.type)
          END

    PROCEDURE factor(VAR x: ORG.Item);
      ...
      IF sym = ORS.ident THEN
        qualident(obj);  
        IF obj.class = ORB.SFunc THEN StandFunc(x, obj.val, obj.type)
        ELSE ORG.MakeItem(x, obj, level); selector(x, sel); CheckTypeCase(x, obj, sel);  (*CheckTypeCase may set x.rdo*)

Writing the actual error message is deferred to *CheckReadOnly*. In the case of *ORP.Parameter*, this is necessary, because *ORP.factor* does *not* return the object designating the case variable. We use the field *x.rdo* to transport the result of the call to *CheckTypeCase* back to the caller.