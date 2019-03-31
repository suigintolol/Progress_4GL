
/* DEFINE QUERY */
DEFINE TEMP-TABLE tt_katalog
    FIELD cod_good LIKE katalog.cod_good
    FIELD NAME LIKE katalog.NAME
    FIELD artic LIKE katalog.artic
    FIELD cod_firm LIKE katalog.cod_firm.
    
DEFINE QUERY q_classf FOR classf.
DEFINE QUERY q_tt_katalog FOR tt_katalog.

/* DEFINE WIDGETS */
/*вывод узлов классификатора*/
DEFINE BROWSE b_classf QUERY q_classf 
    DISPLAY classf.id classf.NAME classf.PARENT_id
    WITH 7 DOWN TITLE "Классификатор".

/*вывод каталога классификатора*/
DEFINE BROWSE b_tt_katalog QUERY q_tt_katalog 
    DISPLAY tt_katalog.cod_good tt_katalog.NAME tt_katalog.artic tt_katalog.cod_firm
    WITH 10 DOWN TITLE "Каталог". 

DEFINE VARIABLE checkboxsort AS LOGICAL FORMAT "TRUE/FALSE" INITIAL FALSE
     LABEL "По убыванию"
     VIEW-AS TOGGLE-BOX SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE var_classf_id AS INTEGER.
DEFINE VARIABLE var_back_classf_id AS INTEGER.

DEFINE VARIABLE comsorted AS CHARACTER FORMAT "X(256)":U
    LABEL "Сортировать по" 
    INITIAL "Наименование"
    VIEW-AS COMBO-BOX
    LIST-ITEMS "Наименование", "Артикул", "Код"
    INNER-LINES 15    
    DROP-DOWN-LIST
    SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE filtr_name AS CHARACTER FORMAT "X(256)":U 
     LABEL "Наименование" 
     VIEW-AS FILL-IN
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE filtr_art AS CHARACTER FORMAT "X(256)":U 
     LABEL "Артикул" 
     VIEW-AS FILL-IN
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE filtr_post AS CHARACTER FORMAT "X(256)":U 
     LABEL "Код поставщика"
     VIEW-AS FILL-IN
     SIZE 20 BY 1 NO-UNDO.

DEFINE BUTTON exit_btn LABEL "Выход.".

/* DEFINE FRAME */
/*содержит таблицу классификатора и каталога*/
DEFINE FRAME f_main
    b_classf HELP "F5-вперёд, F6-назад, F7-показать все товары в выбраном узле, F4-выход."
    b_tt_katalog HELP "F5-фильтр, F6-сортировка, F7-редактировать, F8-удалить, F4-выход."
    exit_btn HELP "F4-Выход."
    WITH 1 COLUMN SIDE-LABELS SIZE 132 BY 40
    CENTERED TITLE "Просмотр каталога по классификатору.".

/*фрейм для фильтра*/
DEFINE FRAME f_katalog_filtr SKIP(1)
    filtr_name HELP "Esc-вернуться применив фильтр, F7-сбросить фильтр."
    filtr_art HELP "Esc-вернуться применив фильтр, F7-сбросить фильтр."
    filtr_post HELP "Esc-вернуться применив фильтр, F7-сбросить фильтр."
    WITH 1 COLUMN SIDE-LABELS SIZE 50 BY 7
    VIEW-AS DIALOG-BOX CENTERED TITLE "Фильтр".

/*фрейм для сортировки*/
DEFINE FRAME f_main_sort SKIP(1)
    comsorted HELP "Esc-вернуться, применив сортировку."
    checkboxsort HELP "Esc-вернуться, применив сортировку." 
    WITH 1 COLUMN SIDE-LABELS SIZE 50 BY 7
    VIEW-AS DIALOG-BOX CENTERED TITLE "Сортировка".

/* MAIN LOGIC */
ENABLE ALL WITH FRAME f_main.
OPEN QUERY q_classf FOR EACH classf WHERE classf.PARENT_id = 0.
comsorted:SCREEN-VALUE = "Наименование".
RUN get_cl-good(0).
RUN print_tt_katalog.
DISPLAY WITH FRAME f_main.

/*выход по классификатору и каталогу*/
ON F4 OF b_classf, b_tt_katalog, exit_btn DO:     
    MESSAGE "Вы действительно хотите выйти" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Выход." UPDATE exit_button AS LOGICAL.
    IF exit_button THEN DO: 
        QUIT.
    END.
END.

/*переход на следующий уровень классификатора*/
ON Enter OF b_classf DO:           
    IF NOT AVAIL classf THEN RETURN.
    var_classf_id = classf.id.
    RUN class_in_the_catalog.
END.

/*переход на предыдущий уровень классификатора*/ 
ON BACKSPACE OF b_classf DO:         
    IF var_classf_id = 0 THEN DO:
        RETURN.
    END.
    FIND FIRST classf WHERE classf.id = var_classf_id.
    var_back_classf_id = var_classf_id.
    var_classf_id = classf.parent_id.
    IF var_classf_id <> 0 THEN DO:
        FIND FIRST classf WHERE classf.id = var_classf_id.
    END.
    RUN class_in_the_catalog.

    REPEAT:
        IF classf.id = var_back_classf_id THEN LEAVE.   
        b_classf:SELECT-NEXT-ROW().
    END.

END.

/*показать все товары по выбранному узлу и по дочерним узлам*/ 
ON F7 OF b_classf DO:
    RUN clear_tt_katalog.
    RUN recursion_search_classf(classf.id).
    RUN print_tt_katalog.
END.

ON F9 OF b_classf DO:


OPEN QUERY q_tt_katalog FOR EACH tt_katalog WHERE cl-good.id = 97 AND
            tt_katalog.NAME MATCHES "*" + filtr_name:SCREEN-VALUE IN FRAME f_katalog_filtr + "*" AND tt_katalog.artic MATCHES "*" +
            filtr_art:SCREEN-VALUE + "*" AND STRING(tt_katalog.cod_firm) MATCHES "*" + 
            filtr_post:SCREEN-VALUE + "*" BY tt_katalog.cod_good Descending.


END.


/*применение фильтров при изменении полей фильтра*/
ON VALUE-CHANGED OF 
    comsorted,
    checkboxsort, 
    filtr_name ,
    filtr_art,
    filtr_post DO:
    RUN print_tt_katalog.
END.

/*обновление каталога при смене записи*/
ON VALUE-CHANGED OF b_tt_katalog DO:
    b_tt_katalog:REFRESH().
END.

/*вызов формы фильтра в каталоге*/
ON F5 OF b_tt_katalog DO: 
    SET filtr_name
        filtr_art
        filtr_post 
        WITH FRAME f_katalog_filtr.
END.

/*очистка полей формы фильтра*/
ON F7 OF
    filtr_name,
    filtr_art,
    filtr_post DO: 
    filtr_name:SCREEN-VALUE = "".
    filtr_art:SCREEN-VALUE = "".
    filtr_post:SCREEN-VALUE = "".
    RUN print_tt_katalog.
END.

/*вызов формы сортировки в каталоге*/
ON F6 OF b_tt_katalog DO:
    SET comsorted
        checkboxsort 
        WITH FRAME f_main_sort.
END.

/*изменение наименования выбранной записи в каталоге*/
ON F7 OF b_tt_katalog DO:
    FIND FIRST katalog EXCLUSIVE-LOCK WHERE tt_katalog.cod_good = katalog.cod_good NO-ERROR.
    IF NOT AVAIL katalog THEN DO:
        MESSAGE "Запись была удалена." VIEW-AS ALERT-BOX TITLE "Внимание!".
        b_tt_katalog:DELETE-SELECTED-ROWS().
        RETURN. 
    END.
    ELSE DO:
        tt_katalog.NAME = katalog.NAME.
                b_tt_katalog:REFRESH().
        UPDATE katalog.NAME HELP "Esc-отмена, Enter-применить изменения." WITH VIEW-AS DIALOG-BOX TITLE "Изменение записи".    
        tt_katalog.NAME = katalog.NAME.
        b_tt_katalog:REFRESH().
    END.
    FIND FIRST katalog NO-LOCK.
END.

/*удаление выбранной записи в каталоге*/
ON F8 OF b_tt_katalog DO:
    MESSAGE "Вы действительно хотите удалить: " + tt_katalog.NAME + " ? " VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Удаление" UPDATE del_button AS LOGICAL.
    IF del_button THEN DO: 
        FIND katalog EXCLUSIVE-LOCK WHERE tt_katalog.cod_good = katalog.cod_good NO-ERROR.
        IF NOT AVAIL katalog THEN DO:
            MESSAGE "Запись была удалена." VIEW-AS ALERT-BOX TITLE "Внимание!".
            b_tt_katalog:DELETE-SELECTED-ROWS().
            RETURN. 
        END.
        FOR EACH cl-good OF katalog:
            DELETE cl-good.
        END.
        DELETE katalog.
        b_tt_katalog:DELETE-SELECTED-ROWS().  
    END.
    ELSE DO:
        RETURN.
    END.
END.

/*
ON F8 OF b_tt_katalog DO:
    GET CURRENT qst EXCLUSIVE-LOCK NO-WAIT.
    IF LOCKED KATALOG THEN MESSAGE "STUDENT LOCKED"
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    ELSE DO:
        ENABLE ALL WITH FRAME upd_frame.
        UPDATE KATALOG.name WITH FRAME upd_frame.
        DISPLAY KATALOG.NAME WITH BROWSE brws_st.
        GET CURRENT qst NO-LOCK.
    END.
END.*/


WAIT-FOR CHOOSE OF exit_btn.

/***************************  PROCEDURE  ************************************/

/*добавляет все товары которые связанны с выбранным классификатором во временную таблицу*/
PROCEDURE print_kat:
    DEFINE INPUT PARAMETER id_cl-good AS decimal.
    FIND FIRST katalog NO-LOCK WHERE katalog.cod_good = id_cl-good NO-ERROR.
    IF AVAIL katalog THEN DO:    
        RUN add_from_kat NO-ERROR.
    END.
END.

/*находит все записи в таблице-связке, которые принадлежат выбранному классификатору*/
PROCEDURE get_cl-good: 
    DEFINE INPUT PARAMETER id_classf AS INTEGER.
    FOR EACH cl-good NO-LOCK WHERE cl-good.id = id_classf:
        RUN print_kat(cl-good.cod_good).
    END.
END.

PROCEDURE get_cl-good2: 
    DEFINE INPUT PARAMETER id_classf AS INTEGER.
    FOR EACH cl-good WHERE cl-good.id = id_classf:
        FIND katalog OF cl-good NO-LOCK NO-ERROR.
        IF AVAIL katalog THEN DO:
            RUN add_from_kat.
        END.
    END.
END.

/*находит все узлы классификатора принадлежащие выбранному классификатору*/
PROCEDURE recursion_search_classf:
    DEFINE INPUT PARAMETER id_classf AS INTEGER.
    DEFINE BUFFER buf_classf FOR classf.
    RUN get_cl-good2(id_classf).
    FIND FIRST buf_classf.
    REPEAT:
        IF buf_classf.parent_id = id_classf THEN DO:
            RUN recursion_search_classf(buf_classf.id).
        END.
        FIND NEXT buf_classf.
    END.
END.

/*упрощает работу классификатора с каталогом*/
PROCEDURE class_in_the_catalog:
    IF var_classf_id = 0 THEN DO:
        b_classf:TITLE IN FRAME f_main = "Классификатор".
    END.
    ELSE DO:
        b_classf:TITLE = classf.name.
    END.
    FIND FIRST classf WHERE var_classf_id = classf.parent_id NO-ERROR.
    IF AVAIL classf THEN DO:
        OPEN QUERY q_classf FOR EACH classf WHERE var_classf_id = classf.parent_id.
    END.
    RUN clear_tt_katalog.
    RUN get_cl-good2(var_classf_id).
    RUN print_tt_katalog.
END PROCEDURE.

/*очистка временной таблицы*/
PROCEDURE clear_tt_katalog:
    FOR EACH tt_katalog:
        DELETE tt_katalog.
    END.
END PROCEDURE.

/*процедура выводит записи из временной таблицы каталога в browse применяя на них сортировку и фильтр*/
PROCEDURE print_tt_katalog:
    IF checkboxsort:SCREEN-VALUE IN FRAME f_main_sort = "TRUE" THEN DO:
        CASE comsorted:SCREEN-VALUE IN FRAME f_main_sort:
        WHEN "Наименование" THEN
            OPEN QUERY q_tt_katalog FOR EACH tt_katalog WHERE
            tt_katalog.NAME MATCHES "*" + filtr_name:SCREEN-VALUE IN FRAME f_katalog_filtr + "*" AND tt_katalog.artic MATCHES "*" + filtr_art:SCREEN-VALUE + "*" AND STRING(tt_katalog.cod_firm) MATCHES "*" + filtr_post:SCREEN-VALUE + "*" BY tt_katalog.NAME Descending.
        WHEN "Артикул" THEN
            OPEN QUERY q_tt_katalog FOR EACH tt_katalog WHERE
            tt_katalog.NAME MATCHES "*" + filtr_name:SCREEN-VALUE + "*" AND tt_katalog.artic MATCHES "*" + filtr_art:SCREEN-VALUE + "*" AND STRING(tt_katalog.cod_firm) MATCHES "*" + filtr_post:SCREEN-VALUE + "*" BY tt_katalog.artic Descending.
        WHEN "Код" THEN
            OPEN QUERY q_tt_katalog FOR EACH tt_katalog WHERE
            tt_katalog.NAME MATCHES "*" + filtr_name:SCREEN-VALUE + "*" AND tt_katalog.artic MATCHES "*" + filtr_art:SCREEN-VALUE + "*" AND STRING(tt_katalog.cod_firm) MATCHES "*" + filtr_post:SCREEN-VALUE + "*" BY tt_katalog.cod_good Descending.
        END CASE.
    END.
    ELSE DO:
        CASE comsorted:SCREEN-VALUE IN FRAME f_main_sort:
        WHEN "Наименование" THEN
            OPEN QUERY q_tt_katalog FOR EACH tt_katalog WHERE
            tt_katalog.NAME MATCHES "*" + filtr_name:SCREEN-VALUE + "*" AND tt_katalog.artic MATCHES "*" + filtr_art:SCREEN-VALUE + "*" AND STRING(tt_katalog.cod_firm) MATCHES "*" + filtr_post:SCREEN-VALUE + "*" BY tt_katalog.name.
        WHEN "Артикул" THEN
            OPEN QUERY q_tt_katalog FOR EACH tt_katalog WHERE
            tt_katalog.NAME MATCHES "*" + filtr_name:SCREEN-VALUE + "*" AND tt_katalog.artic MATCHES "*" + filtr_art:SCREEN-VALUE + "*" AND STRING(tt_katalog.cod_firm) MATCHES "*" + filtr_post:SCREEN-VALUE + "*" BY tt_katalog.artic.
        WHEN "Код" THEN
            OPEN QUERY q_tt_katalog FOR EACH tt_katalog WHERE
            tt_katalog.NAME MATCHES "*" + filtr_name:SCREEN-VALUE + "*" AND tt_katalog.artic MATCHES "*" + filtr_art:SCREEN-VALUE + "*" AND STRING(tt_katalog.cod_firm) MATCHES "*" + filtr_post:SCREEN-VALUE + "*" BY tt_katalog.cod_good.
        END CASE.
    END.
    RETURN.
END PROCEDURE.

/*копирует выбранную запись каталога во временную таблицу*/
PROCEDURE add_from_kat:
    CREATE tt_katalog.
    tt_katalog.cod_good = katalog.cod_good.
    tt_katalog.name = katalog.name.
    tt_katalog.cod_firm = katalog.cod_firm.
    tt_katalog.artic = katalog.artic.
END PROCEDURE.

