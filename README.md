# 📚 Mypandoc





MyPandoc is a project that is made to convert a file in a format to another format. The three formats that needs to be handle are xml, json and markdown. You can export a file to another format with our MyPandoc by following the usage tutorial.


We have three formats, here is an simple example of each formats :

XML Format
```xml
<document>
    <header title="Simple example">
        </header>
    <body>
        <paragraph>This is a simple example</paragraph>
    </body>
</document>
```

JSON Format
```json
{
    "header": {
        "title": "Simple example"
    },
    "body": [
        "This is a simple example"
    ]
}
```

Markdown Format
```markdown
---
title: Simple example
---
This is a simple example
```

## ⚙️ Usage

**`./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]`**

**ifile** : path to the file to convert  
**oformat** : output format (xml, json, markdown)  
**ofile** : path to the output file  
**iformat**: input format (xml, json, markdown)

![mypandoc (1)](https://github.com/EpitechPromo2027/B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala/assets/114906947/99b958c4-e10a-4bd4-b61e-ee6099e22bbb)

We have a core and three parsers (one for each format). All the parsers are using our parsing library. It will send the data into a data structure and the core will convert it to the output format and export it in a file or in the standart output if no output file is given.

![mypandoc](https://github.com/EpitechPromo2027/B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala/assets/114906947/9c28daaf-c78b-4d77-ae0b-bbb44c28af1d)

Our program is very robust. We have more than 250 functionnals tests. It can handle a file with everything in the same line. It can also handle errors files and errors with options.

![image](https://github.com/EpitechPromo2027/B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala/assets/114906947/03c7ea20-e0ad-40fa-b5b4-1f49422a53d2)
