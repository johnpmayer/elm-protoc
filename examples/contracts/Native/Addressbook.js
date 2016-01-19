Elm.Native.Addressbook = Elm.Native.Addressbook || {}
Elm.Native.Addressbook = function(_elm) {
  "use strict";
  _elm.Native.Addressbook = _elm.Native.Addressbook || {}
  if (_elm.Native.Addressbook.values) {
    return _elm.Native.Addressbook.values
  }
  
  // .proto source
  var protoSource = `// See README.txt for information and build instructions.
  
  package addressBook;
  
  message Person {
    required string name = 1;
    required int32 id = 2;        // Unique ID number for this person.
    optional string email = 3;
  
    enum PhoneType {
      MOBILE = 0;
      HOME = 1;
      WORK = 2;
    }
  
    message PhoneNumber {
      required string number = 1;
      optional PhoneType type = 2 [default = HOME];
    }
  
    repeated PhoneNumber phone = 4;
  }
  
  // Our address book file is just one of these.
  message AddressBook {
    repeated Person person = 1;
  }`;
  
  var ProtoBuilder = dcodeIO.ProtoBuf.loadProto(protoSource);
  var Proto = ProtoBuilder.build("addressBook");

  var encodePerson = function(message_Person} {
    return message_Person.toArrayBuffer();
  }
  var decodePerson = function(blob) {
    return Proto.Person.decode(blob);
  }
  var marshalPerson = function(value_Person} {
  }
  var unmarshalPerson = function(message_Person} {
  }
  var encodeAddressBook = function(message_AddressBook} {
    return message_AddressBook.toArrayBuffer();
  }
  var decodeAddressBook = function(blob) {
    return Proto.AddressBook.decode(blob);
  }
  var marshalAddressBook = function(value_AddressBook} {
  }
  var unmarshalAddressBook = function(message_AddressBook} {
  }

  return _elm.Native.Addressbook.values = {
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
