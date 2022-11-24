# SOM

Implementación de Mapa Auto-Organizativo (SOM) en Scala para las prácticas profesionales de 2do año.<br/>

Con la finalidad de evaluar los SOM en la detección de anomalías se añade tal funcionalidad a la red.<br/>

## Contenido

<ul>
  <li> Implementación de SOM para entrenamiento secuencial (on-line) y por lotes (batch). <li/>
  <li> Clases para la representación y manipulación de los vectores de entrada. <li/>
  <li> Contenedor de funciones para flexibilizar la red a varias configuraciones de estas. <br/>
       Se pueden emplear distintas funciones para uno de los conjuntos siguientes
       <ol>
          <li> Inicialización de la red <li/>
          <li> Funciones de distancia <li/>
          <li> Funciones de vecindad <li/>
       </ol>
  </li>
  <li> Clases de manipulación de configuración en la que incluir todos los parámetros necesarios para construir una red </li>
  <li> Clases de manipulación de los parámetros medidos en las pruebas a la red
  <li> Lectura y escritura en memoria externa mediante archivos csv. <br/> 
       Soporta la lectura de datasets y la exportación de los resultados de una prueba. <br/>
       Permite la exportación de una red entrenada y carga de esta a través de archivos .json
  </li>
  <li> Clase con utilidades varias para el uso de la red, como conteo de los clases, muestreos y otras mannipulaciones a los datos </li>
  <li> Controladora con los flujos usados para las diferentes pruebas en la red </li>
</ul>

