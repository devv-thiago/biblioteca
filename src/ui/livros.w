&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME Hwin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Hwin 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-init

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 bt-criar bt-editar bt-pesquisa ~
bt-deletar bt-anterior bt-proximo cod-livro titulo autor qtd-estoq Data-pub 
&Scoped-Define DISPLAYED-OBJECTS cod-livro titulo autor qtd-estoq Data-pub 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE Hwin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-salvar
     LABEL "Salvar"
     SIZE 10 BY 1.52.

DEFINE BUTTON bt-anterior 
     IMAGE-UP FILE "ii-ante.bmp":U
     LABEL "" 
     SIZE 6 BY 1.52.

DEFINE BUTTON bt-criar 
     IMAGE-UP FILE "ii-add.bmp":U
     LABEL "" 
     SIZE 6 BY 1.52.

DEFINE BUTTON bt-deletar 
     IMAGE-UP FILE "ii-era.bmp":U
     LABEL "" 
     SIZE 6 BY 1.52.

DEFINE BUTTON bt-editar 
     IMAGE-UP FILE "ii-comments.bmp":U
     LABEL "" 
     SIZE 6 BY 1.52.

DEFINE BUTTON bt-pesquisa 
     IMAGE-UP FILE "ii-gr-zoo.bmp":U
     LABEL "" 
     SIZE 6 BY 1.52.

DEFINE BUTTON bt-proximo 
     IMAGE-UP FILE "ii-gr-prx.bmp":U
     LABEL "" 
     SIZE 6 BY 1.52.

DEFINE VARIABLE autor AS CHARACTER FORMAT "X(50)":U 
     LABEL "Autor" 
     VIEW-AS FILL-IN 
     SIZE 70 BY 1 NO-UNDO.

DEFINE VARIABLE cod-livro AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Codigo" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE Data-pub AS DATE FORMAT "99/99/9999":U 
     LABEL "Data pub" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE qtd-estoq AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Exemplares" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE titulo AS CHARACTER FORMAT "X(50)":U 
     LABEL "Titulo" 
     VIEW-AS FILL-IN 
     SIZE 70 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 92 BY 2.1
     BGCOLOR 3 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-init
     bt-criar AT ROW 1.33 COL 3 WIDGET-ID 32
     bt-editar AT ROW 1.33 COL 13 WIDGET-ID 34
     bt-pesquisa AT ROW 1.33 COL 23 WIDGET-ID 36
     bt-deletar AT ROW 1.33 COL 43 WIDGET-ID 38
     bt-anterior AT ROW 1.33 COL 72.6 WIDGET-ID 40
     bt-proximo AT ROW 1.33 COL 83 WIDGET-ID 42
     cod-livro AT ROW 3.91 COL 9 COLON-ALIGNED WIDGET-ID 48
     titulo AT ROW 6.29 COL 9 COLON-ALIGNED WIDGET-ID 44
     autor AT ROW 8.14 COL 9 COLON-ALIGNED WIDGET-ID 50
     qtd-estoq AT ROW 10.05 COL 14.8 COLON-ALIGNED WIDGET-ID 54
     Data-pub AT ROW 10.05 COL 59 COLON-ALIGNED WIDGET-ID 52
     bt-salvar AT ROW 12.05 COL 38 COLON-ALIGNED WIDGET-ID 56
     RECT-1 AT ROW 1.05 COL 1 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COLUMN 1 ROW 1
         SIZE 92.2 BY 17.91 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW Hwin ASSIGN
         HIDDEN             = YES
         TITLE              = "Cadastro de livros"
         HEIGHT             = 17.91
         WIDTH              = 92.2
         MAX-HEIGHT         = 17.91
         MAX-WIDTH          = 92.2
         VIRTUAL-HEIGHT     = 17.91
         VIRTUAL-WIDTH      = 92.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW Hwin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-init
   FRAME-NAME                                                           */
ASSIGN autor:READ-ONLY IN FRAME f-init        = TRUE
       cod-livro:READ-ONLY IN FRAME f-init        = TRUE
       Data-pub:READ-ONLY IN FRAME f-init        = TRUE
       qtd-estoq:READ-ONLY IN FRAME f-init        = TRUE
       titulo:READ-ONLY IN FRAME f-init        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(Hwin)
THEN Hwin:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Hwin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Hwin Hwin
ON END-ERROR OF Hwin /* Cadastro de livros */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Hwin Hwin
ON WINDOW-CLOSE OF Hwin /* Cadastro de livros */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-criar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-criar Hwin
ON CHOOSE OF bt-criar IN FRAME f-init
DO:
    
    ASSIGN titulo:READ-ONLY     = FALSE
           autor:READ-ONLY      = FALSE
           qtd-estoq:READ-ONLY  = FALSE
           Data-pub:READ-ONLY   = FALSE
           cod-livro:SCREEN-VALUE = STRING(NEXT-VALUE(seq-livro)).
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-criar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-criar Hwin
ON CHOOSE OF bt-salvar IN FRAME f-init
DO:
        
     CREATE Livros.
     
     ASSIGN Livros.titulo = titulo:SCREEN-VALUE
            Livros.autor = autor:SCREEN-VALUE
            Livros.qtd-estoque = INT(qtd-estoq:SCREEN-VALUE)
            Livros.data-pub = DATE(Data-pub:SCREEN-VALUE)
            Livros.id = INT(cod-livro:SCREEN-VALUE).
     
     RELEASE Livros.
     
     FIND FIRST Livros WHERE Livros.id = INT(cod-livro:SCREEN-VALUE).
     
     IF AVAIL Livros THEN DO:
        MESSAGE "Registro criado com sucesso!" VIEW-AS ALERT-BOX INFORMATION.
     END.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Hwin 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Hwin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(Hwin)
  THEN DELETE WIDGET Hwin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Hwin  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY cod-livro titulo autor qtd-estoq Data-pub 
      WITH FRAME f-init IN WINDOW Hwin.
  ENABLE RECT-1 bt-criar bt-editar bt-pesquisa bt-deletar bt-anterior 
         bt-proximo cod-livro titulo autor qtd-estoq Data-pub bt-salvar
      WITH FRAME f-init IN WINDOW Hwin.
  {&OPEN-BROWSERS-IN-QUERY-f-init}
  VIEW Hwin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

