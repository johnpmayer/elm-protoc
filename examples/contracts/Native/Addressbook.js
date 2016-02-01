Elm.Native.AddressBook = Elm.Native.AddressBook || {};
Elm.Native.AddressBook.make = function(_elm) {
  "use strict";
  _elm.Native.AddressBook = _elm.Native.AddressBook || {};
  if (_elm.Native.AddressBook.values) {
    return _elm.Native.AddressBook.values;
  }

  var Proto = Elm.Native.ElmProto.make(_elm);

  var encodePerson = function(message_Person) {
    return message_Person.serializeBinary()
  }
  var decodePerson = function(blob) {
    return ElmProto.Person.deserializeBinary(blob);
  }
  var encodeAddressBook = function(message_AddressBook) {
    return message_AddressBook.serializeBinary()
  }
  var decodeAddressBook = function(blob) {
    return ElmProto.AddressBook.deserializeBinary(blob);
  }

  return _elm.Native.AddressBook.values = {
    encodePerson: encodePerson,
    decodePerson: decodePerson,
    marshalPerson: marshalPerson,
    unmarshalPerson: unmarshalPerson,
    encodeAddressBook: encodeAddressBook,
    decodeAddressBook: decodeAddressBook,
    marshalAddressBook: marshalAddressBook,
    unmarshalAddressBook: unmarshalAddressBook,
  }
}
