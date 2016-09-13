- title : F# compiler
- description : Excursion into the F# compiler
- author : Steffen Forkmann
- theme : night
- transition : default

***

### Excursion into the F# compiler

Steffen Forkmann

---

### Disclaimer

![PRs](images/PRs.png)

Level: Just enough to be dangerous


***

### What is a compiler?

---

Source code 
 
   -> Compiler
   
-> Program / Error message

---

Source code 
 
   -> Lexer -> Parser -> Type checker => TAST
   
   -> Optimizer -> Code Generator
   
-> Program / Error message

***

### Lexer

* Reads source character by character and returns stream of tokens

---

![Lexer pattern match](images/Lexer.png)

---

<img src="images/tabs.png" alt="Tabs vs. Spaces" >

---

### Tabs vs. Spaces - lex.fsl


    let offwhite = ['\t']

    match token with
    ...
    | offwhite+  
        { if args.lightSyntaxStatus.Status then 
              errorR(Error(FSComp.SR.lexTabsNotAllowed(),lexbuf.LexemeRange))
          if not skip then 
              (WHITESPACE (LexCont.Token !args.ifdefStack)) 
          else 
              token args skip lexbuf }

---

## Pull Request #1243

    let creditCardNumber = 1234_5678_9012_3456L
    let socialSecurityNumber = 999_99_9999L
    let pi = 3.14_15F
    let hexBytes = 0xFF_EC_DE_5E
    let hexWords = 0xCAFE_BABE
    let maxLong = 0x7fff_ffff_ffff_ffffL
    let nybbles = 0b0010_0101
    let bytes = 0b11010010_01101001_10010100_10010010

---

<img src="images/PR1243.png" alt="Underscores in number literals" width=700 >

---

### Pull Request #1243

    // before
    let integer = digit+


    // after
    let integer = digit ((digit | separator)* digit)?


***

### Parser

* Creates syntactic structure (parse tree) from a stream of tokens

---

### Euclidean algorithm

    while b != 0
        if a > b
            a := a − b
        else
            b := b − a
    return a

<img src="images/AST.svg.png" alt="AST from wikipedia" width=300 >

---

### Pull Request #1243

    let inline ( *. )  (x:int64)  (y:int64)  = (# "mul" x y : int64 #)

    // convert to int
    let ParseInt32 (s:string) = 
        if System.Object.ReferenceEquals(s,null) then
            raise( new System.ArgumentNullException("s") )
        let s = removeUnderscores (s.Trim())
        let l = s.Length 
        let mutable p = 0 
        let sign = getSign32 s & p l
        let specifier = get0OXB s & p l 
        if p >= l then formatError() else        
        match Char.ToLowerInvariant(specifier) with
        | 'x' -> 
            sign *. Int64.Parse(
                        s.Substring(p), 
                        NumberStyles.AllowHexSpecifier,
                        CultureInfo.InvariantCulture)
        | 'b' -> sign *. (int64OfUInt64 (parseBinaryUInt64 s p l))
        | 'o' -> sign *. (int64OfUInt64 (parseOctalUInt64 s p l))
        | _ -> Int64.Parse(s, NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture)        

***

### Name resolution


<img src="images/AST.svg.png" alt="AST from wikipedia" width=300 >

---

    open System
    open System.Collections.Generic

    let list = List<_>()
    list.Add 3

    type List = { ... }

    let f list = list.Length

---

### Immutable maps FTW

    [<NoEquality; NoComparison>]
    /// The environment of information used to resolve names
    type NameResolutionEnv =
      { /// Values and Data Tags available by unqualified name 
        eUnqualifiedItems: LayeredMap<string,Item>
        /// Data Tags and Active Pattern Tags available by unqualified name 
        ePatItems: NameMap<Item>
        /// Modules accessible via "." notation. Note this is a multi-map. 
        eModulesAndNamespaces:  NameMultiMap<Tast.ModuleOrNamespaceRef>        
        /// Fully qualified modules and namespaces. 'open' does not change this. 
        eFullyQualifiedModulesAndNamespaces:  NameMultiMap<Tast.ModuleOrNamespaceRef>        
        /// RecdField labels in scope. 
        eFieldLabels: NameMultiMap<Tast.RecdFieldRef>

        /// ...

        /// Extension members by type and name 
        eIndexedExtensionMembers: TyconRefMultiMap<ExtensionMember>
        } 

---

![PRs](images/ErrorReporting.png)

---

### Pull Request #1102


![Record labels](images/recordlabel.png)

---

### Pull Request #1102


![Record labels](images/recordlabel2.png)

---

    /// Resolve a long identifier representing a record field 
    let ResolveFieldPrim (ncenv:NameResolver) nenv ad typ (mp,id:Ident) fields =
        let typeNameResInfo = TypeNameResolutionInfo.Default
        let m = id.idRange
        match mp with 
        | [] -> 
            let lookup() = ...

            if isAppTy ncenv.g typ then 
                match ncenv.InfoReader.TryFindRecdOrClassFieldInfoOfType(id.idText,m,typ) with
                | Some (RecdFieldInfo(_,rfref)) -> 
                    [ResolutionInfo.Empty, FieldResolution(rfref,false)]
                | None ->
                    let typeName = NicePrint.minimalStringOfType nenv.eDisplayEnv typ
                    if isRecdTy ncenv.g typ then               
                        error(SuggestOtherLabelsOfSameRecordType nenv typeName id fields,m)
                    else
                        lookup()
            else 
                lookup()
        | _ -> ...

---

    /// Suggest other labels of the same record
    let SuggestOtherLabelsOfSameRecordType (nenv:NameResolutionEnv) typeName id fields =    
        let labelsOfPossibleRecord =
            nenv.eFieldLabels
            |> Seq.filter (fun kv -> 
                kv.Value 
                |> List.map (fun r -> r.TyconRef.DisplayName)
                |> List.exists ((=) typeName))
            |> Seq.map (fun kv -> kv.Key)
            |> Set.ofSeq

        let givenFields = 
            fields 
            |> List.map (fun fld -> fld.idText) 
            |> List.filter ((<>) id.idText)
            |> Set.ofList

        let predictedLabels = Set.difference labelsOfPossibleRecord givenFields
        let predictions = ErrorResolutionHints.FilterPredictions id.idText predictedLabels

        let errorCode,text = FSComp.SR.nrRecordDoesNotContainSuchLabel(typeName, id.idText)
        errorCode,text + ErrorResolutionHints.FormatPredictions predictions

---

    /// Filters predictions based on edit distance to an unknown identifier.
    let FilterPredictions unknownIdent allPredictions =
        allPredictions
        |> Seq.toList
        |> List.distinct
        |> List.sortBy (fun s -> EditDistance.CalcEditDistance(unknownIdent,s))
        |> take 5

***

### Type checking
#### Hindley-Milner Type Inference Algorithm

---

    type TcEnv =
      { eNameResEnv : NameResolutionEnv 
        eUngeneralizableItems: UngeneralizableItem list
        eCompPath: CompilationPath 
        eAccessPath: CompilationPath         
        eContextInfo : ContextInfo 
        eCallerMemberName : string option
        // ...
        }

    type cenv = 
      { g: TcGlobals
        tcSink: TcResultsSink 
        topCcu: CcuThunk  
        css: ConstraintSolverState
        // ...        
        } 

---

    let UnifyTypes cenv (env: TcEnv) m expectedTy actualTy = 
        ConstraintSolver.AddCxTypeEqualsType env.eContextInfo env.DisplayEnv cenv.css m 
           (tryNormalizeMeasureInType cenv.g expectedTy) 
           (tryNormalizeMeasureInType cenv.g actualTy)

---

    let rec fib n = 
        if n <= 2 then 
            1
        else 
            fib (n - 1) + fib (n - 2)

---

### Pull Request #1149


![type test](images/downcast.png)

---

    let SolveTypSubsumesTypWithReport (csenv:ConstraintSolverEnv) ndeep m trace ty1 ty2 =
        TryD (fun () -> SolveTypSubsumesTypKeepAbbrevs csenv ndeep m trace ty1 ty2)
            (fun res ->
                match csenv.eContextInfo with
                | ContextInfo.RuntimeTypeTest ->
                    // test if we can cast other way around
                    match CollectThenUndo (fun _ -> SolveTypSubsumesTypKeepAbbrevs ...) with 
                    | OkResult _ -> ErrorD(...,ContextInfo.DowncastUsedInsteadOfUpcast)
                    | _ -> ErrorD(...,ContextInfo.NoContext)
                | _ -> ErrorD (...,csenv.eContextInfo)
