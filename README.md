### Prerequisitos
1. Tener GHC (Glasgow Haskell Compiler) instalado
2. Tener Alex, el "Flex" de Haskell
3. Tener Happy, el "Bison" de Haskell

Cabe destacar que Alex y Happy ya son parte de las librerías básicas de Haskell. Por lo tanto, con sólo instalar la última versión de GHC será suficiente
Para descargar GHC, entrar a la siguiente liga:
https://www.haskell.org/ghc/download.html

### Cómo correr

Simplemente corriendo el comando make. Se producirán los archivos Patito.hs,Patito.hi,Scanner.hi, Scanner.hs y el ejecutable patito. Para correr el parser con alguna prueba que usted tenga, ponga el siguiente comando: 
```
    cat NOMBRE_DE_PRUEBA.txt | ./patito
```

Se imprimirá en pantalla el mensaje "Parsing Success: program NOMBRE_DE_SU_PROGRAMA" si y sólo si el programa está léxicamente y sintácticamente correcto.

Para correrlo con algún archivo prueba ya incluído, corra simplemente make prueba1, make prueba2 o make prueba3. Por ejemplo:
```
    make prueba1
```

