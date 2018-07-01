"! Repository dependency
INTERFACE zif_abapgit_dependency PUBLIC.
  TYPES:
    gty_dependency_type TYPE c LENGTH 4,
    gty_unique_name     TYPE string,
    gty_relative_name   TYPE string,
    gty_dependency_list TYPE STANDARD TABLE OF REF TO zif_abapgit_dependency WITH DEFAULT KEY.
  METHODS:
    get_unique_name RETURNING VALUE(rv_name) TYPE gty_unique_name,
    get_relative_name RETURNING VALUE(rv_name) TYPE gty_relative_name,
    get_dependency_type RETURNING VALUE(rv_type) TYPE gty_dependency_type,
    get_dependencies RETURNING VALUE(rt_dependencies) TYPE gty_dependency_list.
ENDINTERFACE.
