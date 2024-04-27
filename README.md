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

![alt text](image-4.png)

We have a core and three parsers (one for each format). All the parsers are using our parsing library. It will send the data into a data structure and the core will convert it to the output format and export it in a file or in the standart output if no output file is given.

![alt text](image-3.png)

Our program is very robust. We have more than 250 functionnals tests. It can handle a file with everything in the same line. It can also handle errors files and errors with options.


![alt text](image-7.png)

