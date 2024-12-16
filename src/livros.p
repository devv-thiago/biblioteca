
/*------------------------------------------------------------------------
    File        : livros.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Thiago Fofano
    Created     : Wed Dec 11 21:50:26 BRT 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
PROCEDURE pCadastrar:
    DEFINE INPUT PARAMETER cTitulo       AS CHARACTER FORMAT "X(50)"         NO-UNDO.
    DEFINE INPUT PARAMETER cAutor        AS CHARACTER FORMAT "X(50)"         NO-UNDO.
    DEFINE INPUT PARAMETER dData-pub     AS DATE      FORMAT "99/99/9999"    NO-UNDO.
    DEFINE INPUT PARAMETER iQtd-estoque  AS INTEGER   FORMAT ">>>>>9"        NO-UNDO. 
    
    CREATE Livros.
    
    ASSIGN  Livros.titulo = cTitulo
            Livros.autor  = cAutor
            Livros.data-pub  = dData-pub
            Livros.qtd-estoque = iQtd-estoque.
    
    RELEASE Livros.
END.

PROCEDURE pEditar:
    DEFINE INPUT PARAMETER iId           AS INTEGER   FORMAT ">>>>>9"        NO-UNDO.
    DEFINE INPUT PARAMETER cTitulo       AS CHARACTER FORMAT "X(50)"         NO-UNDO.
    DEFINE INPUT PARAMETER cAutor        AS CHARACTER FORMAT "X(50)"         NO-UNDO.
    DEFINE INPUT PARAMETER dData-pub     AS DATE      FORMAT "99/99/9999"    NO-UNDO.
    DEFINE INPUT PARAMETER iQtd-estoque  AS INTEGER   FORMAT ">>>>>9"        NO-UNDO. 
    
    FIND FIRST Livros WHERE Livros.id =  iId NO-ERROR.
    
    IF AVAIL Livros THEN DO:
       
       ASSIGN   Livros.titulo = cTitulo
                Livros.autor  = cAutor
                Livros.data-pub  = dData-pub
                Livros.qtd-estoque = iQtd-estoque. 
    END.
    
END.

PROCEDURE pExcluir:
    DEFINE INPUT PARAMETER iId           AS INTEGER   FORMAT ">>>>>9"        NO-UNDO.
    
    FIND FIRST Livros WHERE Livros.id =  iId NO-ERROR.
    
    IF AVAIL Livros THEN DO:
       
       DELETE Livros.
        
    END.
    
END.
