FileDescriptorProto{name =
                      Just "examples\\definitions\\addressbook.proto",
                    package = Just "addressBook", dependency = fromList [],
                    public_dependency = fromList [], weak_dependency = fromList [],
                    message_type =
                      fromList
                        [DescriptorProto{name = Just "Person",
                                         field =
                                           fromList
                                             [FieldDescriptorProto{name = Just "name",
                                                                   number = Just 1,
                                                                   label = Just LABEL_REQUIRED,
                                                                   type' = Just TYPE_STRING,
                                                                   type_name = Nothing,
                                                                   extendee = Nothing,
                                                                   default_value = Nothing,
                                                                   oneof_index = Nothing,
                                                                   json_name = Nothing,
                                                                   options = Nothing,
                                                                   unknown'field =
                                                                     UnknownField (fromList [])},
                                              FieldDescriptorProto{name = Just "id",
                                                                   number = Just 2,
                                                                   label = Just LABEL_REQUIRED,
                                                                   type' = Just TYPE_INT32,
                                                                   type_name = Nothing,
                                                                   extendee = Nothing,
                                                                   default_value = Nothing,
                                                                   oneof_index = Nothing,
                                                                   json_name = Nothing,
                                                                   options = Nothing,
                                                                   unknown'field =
                                                                     UnknownField (fromList [])},
                                              FieldDescriptorProto{name = Just "email",
                                                                   number = Just 3,
                                                                   label = Just LABEL_OPTIONAL,
                                                                   type' = Just TYPE_STRING,
                                                                   type_name = Nothing,
                                                                   extendee = Nothing,
                                                                   default_value = Nothing,
                                                                   oneof_index = Nothing,
                                                                   json_name = Nothing,
                                                                   options = Nothing,
                                                                   unknown'field =
                                                                     UnknownField (fromList [])},
                                              FieldDescriptorProto{name = Just "phone",
                                                                   number = Just 4,
                                                                   label = Just LABEL_REPEATED,
                                                                   type' = Nothing,
                                                                   type_name = Just "PhoneNumber",
                                                                   extendee = Nothing,
                                                                   default_value = Nothing,
                                                                   oneof_index = Nothing,
                                                                   json_name = Nothing,
                                                                   options = Nothing,
                                                                   unknown'field =
                                                                     UnknownField (fromList [])}],
                                         extension = fromList [],
                                         nested_type =
                                           fromList
                                             [DescriptorProto{name = Just "PhoneNumber",
                                                              field =
                                                                fromList
                                                                  [FieldDescriptorProto{name =
                                                                                          Just
                                                                                            "number",
                                                                                        number =
                                                                                          Just 1,
                                                                                        label =
                                                                                          Just
                                                                                            LABEL_REQUIRED,
                                                                                        type' =
                                                                                          Just
                                                                                            TYPE_STRING,
                                                                                        type_name =
                                                                                          Nothing,
                                                                                        extendee =
                                                                                          Nothing,
                                                                                        default_value
                                                                                          = Nothing,
                                                                                        oneof_index
                                                                                          = Nothing,
                                                                                        json_name =
                                                                                          Nothing,
                                                                                        options =
                                                                                          Nothing,
                                                                                        unknown'field
                                                                                          =
                                                                                          UnknownField
                                                                                            (fromList
                                                                                               [])},
                                                                   FieldDescriptorProto{name =
                                                                                          Just
                                                                                            "type",
                                                                                        number =
                                                                                          Just 2,
                                                                                        label =
                                                                                          Just
                                                                                            LABEL_OPTIONAL,
                                                                                        type' =
                                                                                          Nothing,
                                                                                        type_name =
                                                                                          Just
                                                                                            "PhoneType",
                                                                                        extendee =
                                                                                          Nothing,
                                                                                        default_value
                                                                                          =
                                                                                          Just
                                                                                            "HOME",
                                                                                        oneof_index
                                                                                          = Nothing,
                                                                                        json_name =
                                                                                          Nothing,
                                                                                        options =
                                                                                          Nothing,
                                                                                        unknown'field
                                                                                          =
                                                                                          UnknownField
                                                                                            (fromList
                                                                                               [])}],
                                                              extension = fromList [],
                                                              nested_type = fromList [],
                                                              enum_type = fromList [],
                                                              extension_range = fromList [],
                                                              oneof_decl = fromList [],
                                                              options = Nothing,
                                                              reserved_range = fromList [],
                                                              reserved_name = fromList [],
                                                              unknown'field =
                                                                UnknownField (fromList [])}],
                                         enum_type =
                                           fromList
                                             [EnumDescriptorProto{name = Just "PhoneType",
                                                                  value =
                                                                    fromList
                                                                      [EnumValueDescriptorProto{name
                                                                                                  =
                                                                                                  Just
                                                                                                    "MOBILE",
                                                                                                number
                                                                                                  =
                                                                                                  Just
                                                                                                    0,
                                                                                                options
                                                                                                  =
                                                                                                  Nothing,
                                                                                                unknown'field
                                                                                                  =
                                                                                                  UnknownField
                                                                                                    (fromList
                                                                                                       [])},
                                                                       EnumValueDescriptorProto{name
                                                                                                  =
                                                                                                  Just
                                                                                                    "HOME",
                                                                                                number
                                                                                                  =
                                                                                                  Just
                                                                                                    1,
                                                                                                options
                                                                                                  =
                                                                                                  Nothing,
                                                                                                unknown'field
                                                                                                  =
                                                                                                  UnknownField
                                                                                                    (fromList
                                                                                                       [])},
                                                                       EnumValueDescriptorProto{name
                                                                                                  =
                                                                                                  Just
                                                                                                    "WORK",
                                                                                                number
                                                                                                  =
                                                                                                  Just
                                                                                                    2,
                                                                                                options
                                                                                                  =
                                                                                                  Nothing,
                                                                                                unknown'field
                                                                                                  =
                                                                                                  UnknownField
                                                                                                    (fromList
                                                                                                       [])}],
                                                                  options = Nothing,
                                                                  unknown'field =
                                                                    UnknownField (fromList [])}],
                                         extension_range = fromList [], oneof_decl = fromList [],
                                         options = Nothing, reserved_range = fromList [],
                                         reserved_name = fromList [],
                                         unknown'field = UnknownField (fromList [])},
                         DescriptorProto{name = Just "AddressBook",
                                         field =
                                           fromList
                                             [FieldDescriptorProto{name = Just "person",
                                                                   number = Just 1,
                                                                   label = Just LABEL_REPEATED,
                                                                   type' = Nothing,
                                                                   type_name = Just "Person",
                                                                   extendee = Nothing,
                                                                   default_value = Nothing,
                                                                   oneof_index = Nothing,
                                                                   json_name = Nothing,
                                                                   options = Nothing,
                                                                   unknown'field =
                                                                     UnknownField (fromList [])}],
                                         extension = fromList [], nested_type = fromList [],
                                         enum_type = fromList [], extension_range = fromList [],
                                         oneof_decl = fromList [], options = Nothing,
                                         reserved_range = fromList [], reserved_name = fromList [],
                                         unknown'field = UnknownField (fromList [])}],
                    enum_type = fromList [], service = fromList [],
                    extension = fromList [], options = Nothing,
                    source_code_info = Nothing, syntax = Just "proto2",
                    unknown'field = UnknownField (fromList [])}