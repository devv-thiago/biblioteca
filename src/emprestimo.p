
/*------------------------------------------------------------------------
    File        : emprestimo.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tfofa
    Created     : Thu Dec 12 19:12:45 BRT 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
PROCEDURE pCadastrar:
    DEFINE INPUT PARAMETER iId-usuario      AS INTEGER   FORMAT ">>>>>9"        NO-UNDO.
    DEFINE INPUT PARAMETER iId-livro        AS INTEGER   FORMAT ">>>>>9"        NO-UNDO.
    DEFINE INPUT PARAMETER dData-emp        AS DATE      FORMAT "99/99/9999"    NO-UNDO.
    DEFINE INPUT PARAMETER dData-prev-dev   AS DATE      FORMAT "99/99/9999"    NO-UNDO.

    
    CREATE Emprestimos.
    
    ASSIGN  Emprestimos.id-usuario      = iId-usuario 
            Emprestimos.id-livro        = iId-livro
            Emprestimos.data-emp        = dData-emp
            Emprestimos.data-prev-dev   = dData-prev-dev.
    
    RELEASE Emprestimos.
END.

PROCEDURE pEditar:
    DEFINE INPUT PARAMETER iId              AS INTEGER   FORMAT ">>>>>9"        NO-UNDO.
    DEFINE INPUT PARAMETER iId-usuario      AS INTEGER   FORMAT ">>>>>9"        NO-UNDO.
    DEFINE INPUT PARAMETER iId-livro        AS INTEGER   FORMAT ">>>>>9"        NO-UNDO.
    DEFINE INPUT PARAMETER dData-emp        AS DATE      FORMAT "99/99/9999"    NO-UNDO.
    DEFINE INPUT PARAMETER dData-prev-dev   AS DATE      FORMAT "99/99/9999"    NO-UNDO.
    
    FIND FIRST Emprestimos WHERE Emprestimos.id =  iId NO-ERROR.
    
    IF AVAIL Emprestimos THEN DO:
       
        ASSIGN  Emprestimos.id-usuario      = iId-usuario 
                Emprestimos.id-livro        = iId-livro
                Emprestimos.data-emp        = dData-emp
                Emprestimos.data-prev-dev   = dData-prev-dev.
    END.
    
END.

PROCEDURE pExcluir:
    DEFINE INPUT PARAMETER iId           AS INTEGER   FORMAT ">>>>>9"        NO-UNDO.
    
    FIND FIRST Emprestimos WHERE Emprestimos.id =  iId NO-ERROR.
    
    IF AVAIL Emprestimos THEN DO:
       
       DELETE Emprestimos.
        
    END.
    
END.