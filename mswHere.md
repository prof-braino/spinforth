mswHere ( -- addr )

A shared word pointer pointing to the current end of the dictionary. When words are being compiled this pointer point to the next free word in the dictionary. It is used to enter data and word addresses into the dictionary. It should only be used and data should only be entered into the dictionary when the dictionary is locked.