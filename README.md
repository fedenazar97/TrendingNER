## POO TrendingNER

### TrendingTopics

## Configuración inicial

* Instalar Java 8.
  * Comprobar si lo tienen instalado y si es la versión correcta `$ java -v`
  * En ubuntu, se instala sólo con `sudo apt install openjdk-8-jdk`
  * Comprobar la variable de entorno `$JAVA_HOME`. Instrucciones [acá](https://docs.opsgenie.com/docs/setting-java_home)

* Instalar scala 2
  * https://docs.scala-lang.org/getting-started/index.html


### TrendingNER

Trendingner es un programa que cuenta NERs con un modelo heurístico sencillo.
La entrada del programa es un archivo json con suscripciones con el siguiente formato:

```json
[
    {
        "url": "URL_TEMPLATE",
        "urlParams": ["PARAM1", "PARAM2"],
        "urlType": "rss or reddit"
    }
]
```
El programa imprime las entidades nombradas.

#### Ejecución

Para ejecutarlo, utilicen el siguientes comandos:

```bash
$ sbt run
```

## Estilos

Para corroborar el estilo de código, ejecutar:
```bash
$ sbt scalastyle
```