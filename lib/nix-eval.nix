rec {
  asSexpr = value:
    let
      type = builtins.typeOf value;
      fullType =
        if type == "set"
	then if value ? type && value.type == "derivation"
	     then "derivation"
	     else "set"
	else type;
    in
      sexprFormat fullType value;

  sexprFormat = type: value:
    if type == "string"
      then builtins.toJSON value
    else if type == "list"
      then builtins.concatStringsSep ""
        (builtins.concatLists [
          ["("]
          [(builtins.concatStringsSep " " (builtins.map asSexpr value))]
          [")"]
        ])
    else if type == "derivation"
      then builtins.concatStringsSep "" ["(derivation " (asSexpr value.outPath) ")"]
    else if type == "set"
      then builtins.concatStringsSep "" (builtins.concatLists [
        ["("]
        [(builtins.concatStringsSep " "
          (builtins.map
            (name: builtins.concatStringsSep "" [
              "("
              name
              " . "
              (asSexpr value.${name})
              ")"
            ])
            (builtins.attrNames value)))]
        [")"]
      ])
    else type;
}