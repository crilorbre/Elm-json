# Elm: COVID-19 

Esto proyecto realizado en Elm, es una demo la cual nos permitirá poder visualizar la información referente al **COVID-19** en todos los paises del mundo de una manera más fácil y ordenada.

## Ejecución

En este apartado, explicaremos los pasos que hay que llevar a cabo para la correcta ejecución del proyecto.

### Pre-requisitos

En primer lugar, deberemos instalar el editor de código de Elm, el cual se puede encontrar en la siguiente [dirección](https://guide.elm-lang.org/install/elm.html).

### Lanzamiento

Una vez realizado el paso anterior, tendremos que:

<ol>

  <li>Clonar el contenido del repositorio o bien descargalo para finalmente descomprimir el ZIP.</li>
  
  <li>
    Ejecutar el siguiente comando desde la carpeta de nuestro proyecto para instalar todas las dependencias utilizadas en el proyecto.
    
    elm make
    
  </li>
  
  <li>
    Ejecutar el siguiente comando  desde la carpeta de nuestro proyecto para levantar nuestro proyecto de Elm.
    
    elm reactor
    
  </li>
  <li>
    Acceder a la siguiente dirección para poder visualizar nuestro proyecto.
    
    http://localhost:8000/src/Covid.elm
    
  </li>
</ol>

## Herramientas

* [Elm](https://elm-lang.org/) - El editor de código de Elm.
* [Visual Studio Code](https://code.visualstudio.com/) - Editor de código fuente.

## Referencias

* [API COVID-19](https://documenter.getpostman.com/view/11203393/SzfAz776?version=latest) - API de donde he obtenido los datos que he utilizado en el proyecto.
* [Estructura del proyecto](https://elmprogramming.com/decoding-json-part-2.html) - Estructura que he seguido para llevar a cabo el proyecto.
* [Paginación de los datos mostrados](https://package.elm-lang.org/packages/jschomay/elm-paginate/latest/) - Ejemplo de refernecia para llevar a cabo la paginación.
