FileDescriptorProto{name =
                      Just "examples\\definitions\\world.proto",
                    package = Just "world", dependency = fromList ["ship.proto"],
                    public_dependency = fromList [], weak_dependency = fromList [],
                    message_type =
                      fromList
                        [DescriptorProto{name = Just "Snapshot",
                                         field =
                                           fromList
                                             [FieldDescriptorProto{name = Just "ships",
                                                                   number = Just 1,
                                                                   label = Just LABEL_REPEATED,
                                                                   type' = Nothing,
                                                                   type_name = Just "ship.Ship",
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
                                         unknown'field = UnknownField (fromList [])},
                         DescriptorProto{name = Just "GameUpdate",
                                         field =
                                           fromList
                                             [FieldDescriptorProto{name = Just "focusEntityId",
                                                                   number = Just 1,
                                                                   label = Just LABEL_OPTIONAL,
                                                                   type' = Just TYPE_UINT64,
                                                                   type_name = Nothing,
                                                                   extendee = Nothing,
                                                                   default_value = Nothing,
                                                                   oneof_index = Just 0,
                                                                   json_name = Nothing,
                                                                   options = Nothing,
                                                                   unknown'field =
                                                                     UnknownField (fromList [])},
                                              FieldDescriptorProto{name = Just "snapshot",
                                                                   number = Just 2,
                                                                   label = Just LABEL_OPTIONAL,
                                                                   type' = Nothing,
                                                                   type_name = Just "Snapshot",
                                                                   extendee = Nothing,
                                                                   default_value = Nothing,
                                                                   oneof_index = Just 0,
                                                                   json_name = Nothing,
                                                                   options = Nothing,
                                                                   unknown'field =
                                                                     UnknownField (fromList [])}],
                                         extension = fromList [], nested_type = fromList [],
                                         enum_type = fromList [], extension_range = fromList [],
                                         oneof_decl =
                                           fromList
                                             [OneofDescriptorProto{name = Just "update",
                                                                   unknown'field =
                                                                     UnknownField (fromList [])}],
                                         options = Nothing, reserved_range = fromList [],
                                         reserved_name = fromList [],
                                         unknown'field = UnknownField (fromList [])}],
                    enum_type = fromList [], service = fromList [],
                    extension = fromList [], options = Nothing,
                    source_code_info = Nothing, syntax = Nothing,
                    unknown'field = UnknownField (fromList [])}