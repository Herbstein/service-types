# service-types

The program loads the file `definitions.json` from the working directory and parses it.

Each `{ "name": "", "urls": []}` object defines a file in which class declarations are put. The name of the file is contained in the `name` field, while the list of classes contained inside the file is listed in the `urls` field.

