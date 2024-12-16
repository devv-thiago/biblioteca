
/*------------------------------------------------------------------------
    File        : usuario.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tfofa
    Created     : Thu Dec 12 19:36:00 BRT 2024
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
PROCEDURE pCadastrar:
    DEFINE INPUT PARAMETER cNome       AS CHARACTER FORMAT "X(100)"        NO-UNDO.
    DEFINE INPUT PARAMETER cCpf        AS CHARACTER FORMAT "X(22)"         NO-UNDO.
    DEFINE INPUT PARAMETER dData-nasc  AS DATE      FORMAT "99/99/9999"    NO-UNDO.
    
    CREATE Usuarios.
    
    ASSIGN  Usuarios.nome      = cNome 
            Usuarios.cpf       = cCpf
            Usuarios.data-nasc = dData-nasc.
    
    RELEASE Usuarios.
END.

PROCEDURE pEditar:
    DEFINE INPUT PARAMETER iId         AS INTEGER   FORMAT ">>>>>9"        NO-UNDO.
    DEFINE INPUT PARAMETER cNome       AS CHARACTER FORMAT "X(100)"        NO-UNDO.
    DEFINE INPUT PARAMETER cCpf        AS CHARACTER FORMAT "X(22)"         NO-UNDO.
    DEFINE INPUT PARAMETER dData-nasc  AS DATE      FORMAT "99/99/9999"    NO-UNDO.
    
    FIND FIRST Usuarios WHERE Usuarios.id =  iId NO-ERROR.
    
    IF AVAIL Usuarios THEN DO:
       
        ASSIGN  Usuarios.nome      = cNome 
                Usuarios.cpf       = cCpf
                Usuarios.data-nasc = dData-nasc.           
    END.
    
END.

PROCEDURE pExcluir:
    DEFINE INPUT PARAMETER iId           AS INTEGER   FORMAT ">>>>>9"        NO-UNDO.
    
    FIND FIRST Usuarios WHERE Usuarios.id =  iId NO-ERROR.
    
    IF AVAIL Usuarios THEN DO:
       
       DELETE Usuarios.
        
    END.
    
END.