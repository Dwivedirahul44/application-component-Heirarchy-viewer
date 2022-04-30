REPORT sy-repid.
*----------------------------------------------------------------------*
*       CLASS lcl_main DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_err DEFINITION INHERITING FROM cx_static_check FINAL.

  PUBLIC SECTION.
    DATA:
      v_message TYPE string.
    METHODS:
      constructor IMPORTING iv_message TYPE string OPTIONAL.

ENDCLASS.                    "lcx_err DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_main DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_main DEFINITION FINAL CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_data,
        node     TYPE seu_id,
        type     TYPE snodetext-type,
        descript TYPE string,
      END OF ts_data,

      BEGIN OF ty_nodekey,
        node     TYPE seu_id,
        key      TYPE salv_de_node_key,
      END OF ty_nodekey,

      BEGIN OF ty_tadir,
        object   TYPE TROBJTYPE,
        obj_name TYPE sobj_name,
        devclass TYPE devclass,
      END OF ty_Tadir.

    TYPES:
      tty_tadir  TYPE STANDARD TABLE OF ty_tadir WITH DEFAULT KEY,
      tt_data    TYPE TABLE OF ts_data.

    CLASS-DATA:
      o_self     TYPE REF TO lcl_main.

    CLASS-DATA:
      v_objtype  TYPE trobjtype.

    CLASS-METHODS:
      init,
      set_screen_data.

    METHODS:
      constructor               RAISING lcx_err,

      start                   IMPORTING iv_objname TYPE obj_name
                                        it_sobjtyp TYPE DEVTYRANGE,

      handle_expand           FOR EVENT if_salv_events_tree~expand_empty_folder
                                     OF cl_salv_events_tree
                              IMPORTING node_key.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      v_obj       TYPE obj_name.

    DATA:
      o_tree      TYPE REF TO cl_salv_tree.

    DATA:
      t_table     TYPE tt_data,
      t_nodekey   TYPE STANDARD TABLE OF ty_nodekey,
      t_comp_data TYPE STANDARD TABLE OF snodetext,
      t_objtype   TYPE RANGE OF TROBJTYPE.

    METHODS:
      ret_tadir               RETURNING VALUE(rt_tadir) TYPE tty_tadir
                                RAISING lcx_err,

      ret_tadir_4_devclass    IMPORTING iv_name  TYPE devclass
                              RETURNING VALUE(rt_tadir) TYPE tty_tadir
                                RAISING lcx_err,

      get_component_data        RAISING lcx_err,

      fill_parent_node          RAISING lcx_err,

      handle_display,

      fill_obj_tree_hierarchy IMPORTING iv_objname TYPE obj_name
                                RAISING lcx_err,

      fill_hf_tree_hierarchy    RAISING lcx_err,

      add_node                IMPORTING is_comp_data TYPE snodetext
                                RAISING lcx_err,

      add_nodes               IMPORTING it_comp_data TYPE snodetab
                                RAISING lcx_err,

      init_tree                 RAISING lcx_err.


ENDCLASS.  "lcl_main
* Types for structure and table of tree data

PARAMETERS:
  p_obj         TYPE obj_name .

SELECT-OPTIONS:
  s_objtyp      FOR  lcl_main=>v_objtype NO INTERVALS.

INITIALIZATION.

  lcl_main=>init(  ).

  lcl_main=>set_screen_data( ).

START-OF-SELECTION.

  lcl_main=>o_self->start( iv_objname = p_obj
                           it_sobjtyp = s_objtyp[] ).


CLASS lcl_main IMPLEMENTATION.
  METHOD set_screen_data .

    %_p_obj_%_app_%-text    = 'Component/Object Name'.
    %_s_objtyp_%_app_%-text = 'Object Types'.

    s_objtyp-low = 'DEVC'.
    APPEND s_objtyp TO s_objtyp[].

    s_objtyp-sign = 'I'.
    s_objtyp-option = 'EQ'.

    s_objtyp-low = 'TRAN'.
    APPEND s_objtyp TO s_objtyp[].

    s_objtyp-low = 'PROG'.
    APPEND s_objtyp TO s_objtyp[].

    s_objtyp-low = 'FUGR'.
    APPEND s_objtyp TO s_objtyp[].

    s_objtyp-low = 'TABL'.
    APPEND s_objtyp TO s_objtyp[].



  ENDMETHOD.
  METHOD handle_display .
    DATA:lo_columns       TYPE REF TO cl_salv_columns,
         lo_column        TYPE REF TO cl_salv_column,
         lo_tree_settings TYPE REF TO cl_salv_tree_settings.
    TRY.

        lo_columns = me->o_tree->get_columns( ).

        lo_column = lo_columns->get_column( 'NODE' ).

        lo_column->set_visible( abap_false ).
        lo_column->set_optimized( abap_false ).

        lo_column = lo_columns->get_column( 'TYPE' ).

        lo_column->set_optimized( abap_true ).

        lo_column = lo_columns->get_column( 'DESCRIPT' ).

        lo_column->set_medium_text( 'Description' ).
        lo_column->set_optimized( abap_true ).

        lo_tree_settings = me->o_tree->get_tree_settings( ).

        lo_tree_settings->set_hierarchy_header( 'Node' ).
        lo_tree_settings->set_hierarchy_size( 40 ).

*     Display Table
        me->o_tree->display( ).
      CATCH cx_salv_not_found.

    ENDTRY.
  ENDMETHOD.                    "handle_display

  METHOD fill_parent_node .
    DATA:
      ls_nodetab TYPE snodetext.

    READ TABLE me->t_comp_data INTO ls_nodetab  WITH KEY parent = '000000'.

    IF syst-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_err
        EXPORTING
          iv_message = 'Error adding parent node.'.
    ENDIF.

    me->add_node( ls_nodetab ).

  ENDMETHOD.                    "fill_parent_node
  METHOD add_node .
    DATA:
      ls_data     TYPE ts_data,
      ls_nodekey  TYPE ty_nodekey.
    DATA:
      lo_node     TYPE REF TO cl_salv_node.
    DATA:
      lv_relation TYPE salv_de_node_relation,
      lv_folder   TYPE abap_bool,
      lv_icon     TYPE salv_de_tree_image,
      lv_text     TYPE lvc_value.

    TRY.

        READ TABLE me->t_nodekey INTO ls_nodekey WITH KEY node = is_comp_data-parent.

        IF syst-subrc = 0.
          lv_relation =   if_salv_c_node_relation=>last_child.
        ELSE.
          lv_relation =   if_salv_c_node_relation=>parent.
        ENDIF.

        IF is_comp_data-type = 'DEVC'
        OR is_comp_data-type = 'HF'.
          lv_folder = abap_true.
        ENDIF.

        CASE is_comp_data-type.
          WHEN 'TABL'.
            lv_icon = icon_database_table.
          WHEN 'FUGR'.
            lv_icon = icon_object_folder.
          WHEN 'DEVC'.
            lv_icon = icon_package_standard.
          WHEN 'TRAN'.
            lv_icon = icon_oo_object.
          WHEN 'PROG'.
            lv_icon = icon_execute_object.
          WHEN OTHERS.
        ENDCASE.

        ls_data-node     = is_comp_data-id.
        ls_data-descript = is_comp_data-text.
        ls_data-type     = is_comp_data-type.

        lv_text = is_comp_data-name.

        lo_node = me->o_tree->get_nodes( )->add_node( related_node = ls_nodekey-key
                                                          expander = lv_folder
                                                    collapsed_icon = lv_icon
                                                     expanded_icon = lv_icon
                                                            folder = lv_folder
                                                          data_row = ls_data
                                                              text = lv_text
                                                      relationship = if_salv_c_node_relation=>last_child
                                                    ).


        ls_nodekey-key  = lo_node->get_key( ).
        ls_nodekey-node = is_comp_data-id.

        APPEND ls_nodekey TO me->t_nodekey.

      CATCH cx_salv_msg.
        RAISE EXCEPTION TYPE lcx_err
          EXPORTING
            iv_message = 'Error Adding node to tree.'.
    ENDTRY.

  ENDMETHOD.                    "add_node

  METHOD add_nodes .
    DATA:
      ls_comp_data TYPE snodetext.
    LOOP AT it_comp_data INTO ls_comp_data FROM 2.

      me->add_node( ls_comp_data ).
    ENDLOOP.

  ENDMETHOD.                    "add_nodes
  METHOD ret_tadir .

    SELECT object
           obj_name
           devclass
           INTO TABLE rt_tadir
           FROM tadir
          WHERE pgmid    = 'R3TR'
            AND obj_name = me->v_obj
            AND object   IN me->t_objtype.

  IF syst-subrc <> 0.
    RAISE EXCEPTION TYPE lcx_err.
  ENDIF.
  ENDMETHOD.
  METHOD fill_obj_tree_hierarchy .
    DATA:
      ls_comp_data  TYPE snodetext,
      ls_comp_data1 TYPE snodetext,
      ls_tadir      TYPE me->ty_tadir.
    DATA:
      lt_comp_data  TYPE STANDARD TABLE OF snodetext,
      lt_tadir      TYPE me->tty_tadir.

      try.

      LOOP AT me->ret_tadir( ) INTO ls_tadir.

        LOOP AT me->t_comp_data INTO ls_comp_data WHERE name  = ls_tadir-devclass.

          IF ls_comp_data-name = iv_objname.
            APPEND ls_comp_data TO lt_comp_data.
            CONTINUE.
          ENDIF.

          ls_comp_data-parent = ls_comp_data-id.
          ls_comp_data-id = ls_comp_data-id + 20000.
          ls_comp_data-name = iv_objname.
          ls_comp_data-text = iv_objname.
          ls_comp_data-type = ls_tadir-object.
          APPEND ls_comp_data TO lt_comp_data.
        ENDLOOP.

      ENDLOOP.

      IF lt_comp_data IS INITIAL.
        RAISE EXCEPTION TYPE lcx_err
          EXPORTING
            iv_message = 'No data found.'.
      ENDIF.

    CATCH lcx_err.

      READ TABLE me->t_comp_data INTO ls_comp_data WITH KEY name = iv_objname.

      IF syst-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_err
          EXPORTING
            iv_message = 'No data found.'.
      ENDIF.

      APPEND ls_comp_data TO lt_comp_data.

    endtry.

    SORT lt_comp_data BY parent type name.
    DELETE ADJACENT DUPLICATES FROM lt_comp_data COMPARING parent
                                                           type
                                                           name.

    LOOP AT lt_comp_data INTO ls_comp_data1 WHERE name = iv_objname.
      ls_comp_data = ls_comp_data1.
      DO .
        READ TABLE me->t_comp_data INTO ls_comp_data WITH KEY id = ls_comp_data-parent.

        IF syst-subrc = 0.
          APPEND ls_comp_data TO lt_comp_data.
        ELSE.
          EXIT.
        ENDIF.

      ENDDO.
    ENDLOOP.
    SORT lt_comp_data BY parent ASCENDING
                             id ASCENDING
                           type ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_comp_data COMPARING parent
                                                           id
                                                           type.

    me->add_nodes( lt_comp_data ).
    me->o_tree->get_nodes( )->expand_all( ).

  ENDMETHOD.                    "fill_Table_tree_hierarchy
  METHOD fill_hf_tree_hierarchy .

    DATA:
      lt_comp_data TYPE STANDARD TABLE OF snodetext.


    lt_comp_data  = me->t_comp_data.

    DELETE lt_comp_data WHERE tlevel > '03'.

    SORT lt_comp_data BY parent ASCENDING
                         id     ASCENDING
                         type   ASCENDING.

    DELETE ADJACENT DUPLICATES FROM lt_comp_data COMPARING parent
                                                           id
                                                           type.

    me->add_nodes( lt_comp_data ).

  ENDMETHOD.                    "fill_Table_tree_hierarchy
  METHOD start .
    DATA:
      lo_event TYPE REF TO cl_salv_events_tree,
      lo_err   TYPE REF TO lcx_err.

    TRY.
        me->v_obj = iv_objname.
        me->t_objtype = it_sobjtyp.

        me->init_tree( ).
        me->fill_parent_node( ).

        IF iv_objname IS NOT INITIAL.
          me->fill_obj_tree_hierarchy( iv_objname ).
        ELSE.
          me->fill_hf_tree_hierarchy( ).
        ENDIF.

        lo_event = me->o_tree->get_event( ).

        SET HANDLER me->handle_expand FOR lo_event.

        me->handle_display( ).

      CATCH lcx_err INTO lo_err.
        MESSAGE e000(db) WITH lo_err->v_message.
    ENDTRY.

  ENDMETHOD.                    "start
  METHOD init_tree .
    TRY.

        cl_salv_tree=>factory( IMPORTING r_salv_tree = me->o_tree
                                CHANGING t_table     = me->t_table
                             ).
      CATCH cx_salv_error.
        RAISE EXCEPTION TYPE lcx_err
          EXPORTING
            iv_message = 'Error initiating Tree.'.
    ENDTRY.

  ENDMETHOD.                    "init_tree
  METHOD ret_tadir_4_devclass .

  select object
         obj_name
         devclass
         INTO TABLE rt_tadir
         FROM tadir
         WHERE tadir~pgmid    = 'R3TR'
           AND tadir~object   IN s_objtyp
           AND tadir~devclass = iv_name.

  IF syst-subrc <> 0.
    RAISE EXCEPTION TYPE lcx_err.
  ENDIF.

  ENDMETHOD.
  METHOD handle_expand .
    DATA:
      ls_comp_data TYPE snodetext,
      ls_tadir     TYPE me->ty_tadir,
      ls_nodekey   TYPE me->ty_nodekey.

    DATA:
      lt_tadir     TYPE STANDARD TABLE OF tadir.

    DATA:
      lv_string    TYPE string,
      lv_length    TYPE i.

    TRY.

        READ TABLE me->t_nodekey INTO ls_nodekey WITH KEY key  = node_key.
        IF syst-subrc <> 0.
          RAISE EXCEPTION TYPE lcx_err.
        ENDIF.

        READ TABLE me->t_comp_data INTO ls_comp_data WITH KEY id = ls_nodekey-node.
        IF syst-subrc <> 0.
          RAISE EXCEPTION TYPE lcx_err.
        ENDIF.

        READ TABLE me->t_comp_data TRANSPORTING NO FIELDS WITH KEY parent = ls_nodekey-node.

        IF syst-subrc = 0.

          LOOP AT me->t_comp_data INTO ls_comp_data WHERE parent = ls_nodekey-node.

            me->add_node( ls_comp_data ).

            lv_string = ls_comp_data-name.

            IF strlen( lv_string ) > lv_length.
              lv_length = strlen( lv_string ).
            ENDIF.

          ENDLOOP.

        elseIF ls_comp_data-type = 'DEVC'.

          try .

            LOOP AT me->ret_tadir_4_devclass( ls_comp_data-name ) INTO ls_tadir.

              ls_comp_data-parent = ls_nodekey-node.
              ls_comp_data-id     = ls_comp_data-id + 20000.
              ls_comp_data-name   = ls_tadir-obj_name.
              ls_comp_data-text   = ls_tadir-obj_name.
              ls_comp_data-type   = ls_tadir-object.

              me->add_node( ls_comp_data ).

              lv_string = ls_comp_data-name.

              IF strlen( lv_string ) > lv_length.
                lv_length = strlen( lv_string ).
              ENDIF.

            ENDLOOP.
            catch lcx_err.
              MESSAGE s000(db) WITH 'No child exists.'.
            ENDTRY.

        ENDIF.

        IF  me->o_tree->get_tree_settings( )->get_hierarchy_size( ) < 50.

          me->o_tree->get_tree_settings( )->set_hierarchy_size( me->o_tree->get_tree_settings( )->get_hierarchy_size( ) + lv_length ).

        ENDIF.

      CATCH cx_salv_msg
            lcx_err.
        MESSAGE s000(db) WITH 'Error adding Nodes' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.                    "handle_expand
  METHOD get_component_data .

    CALL FUNCTION 'RS_COMPONENT_VIEW'
      EXPORTING
        language            = syst-langu
        object_type         = 'TABL'
        ignore_sfw_switches = 'X'
      TABLES
        nodetab             = me->t_comp_data
      EXCEPTIONS
        OTHERS              = 1.

    IF syst-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_err
        EXPORTING
          iv_message = 'Error getting component data.'.
    ENDIF.
    SORT me->t_comp_data BY parent ASCENDING
                            id     ASCENDING.
  ENDMETHOD.                    "get_component_Data
  METHOD init .
    DATA:
      lo_err  TYPE REF TO lcx_err.
    TRY.

        CREATE OBJECT lcl_main=>o_self.

      CATCH lcx_err INTO lo_err.
        MESSAGE s000(db) WITH lo_err->v_message DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.                    "init
  METHOD constructor .

    me->get_component_data( ).


  ENDMETHOD.                    "constructor
ENDCLASS.                    "lcl_main IMPLEMENTATION
"lcl_error DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcx_err IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_err IMPLEMENTATION .

  METHOD constructor.

    super->constructor( ).

    v_message = iv_message.

  ENDMETHOD.                    "constructor

ENDCLASS.
