# 🚀 Incremento de Salarios - Migración Mainframe a Spring Batch

## 📋 Descripción

Este proyecto es una **migración completa** del proceso batch mainframe **PB0EC319** a **Spring Batch con Java**.

### Proceso original (Mainframe):
- **JCL:** INCSAL.JCL
- **Programa COBOL:** PB0EC319 (Cruce.cbl)
- **Tipo:** Matching 1:1 entre ficheros secuenciales

### Proceso migrado (Java):
- **Framework:** Spring Batch
- **Lenguaje:** Java 17
- **Arquitectura:** Reader → Processor → Writer

---

## 🎯 ¿Qué hace este proceso?

Realiza un **cruce de ficheros** para actualizar salarios de empleados:

1. **Lee** el maestro de empleados (25 empleados en el ejemplo)
2. **Busca** si cada empleado tiene una subida salarial pendiente
3. **Actualiza** el salario si existe subida
4. **Genera** un fichero de salida con los datos actualizados
5. **Emite** un informe con estadísticas de ejecución

---

## 📥 Entradas

### 1. Maestro de empleados (`maestro-empleados.csv`)
```
codigo,nombre,apellidos,departamento,salario,fechaAlta,categoria,situacion
00001,JUAN,GARCIA LOPEZ,INFORMATICA,35000.00,2020-01-15,SENIOR,ACTIVO
00002,MARIA,MARTINEZ RUIZ,CONTABILIDAD,32000.00,2019-03-22,SENIOR,ACTIVO
...
```

**Equivalente COBOL:**
```cobol
SELECT MAESTRO ASSIGN TO ...
ORGANIZATION IS INDEXED
ACCESS MODE IS SEQUENTIAL
RECORD KEY IS CLAVE
```

### 2. Fichero de subidas (`subidas.csv`)
```
codigo,importe,motivo
00001,2500.00,REVISION ANUAL
00003,3000.00,PROMOCION
...
```

**Equivalente COBOL:**
```cobol
SELECT SUBIDAS ASSIGN TO ...
ORGANIZATION IS SEQUENTIAL
ACCESS MODE IS SEQUENTIAL
```

---

## 📤 Salida

### Fichero generado (`maestro-empleados-actualizado.csv`)

Contiene el maestro completo con los salarios actualizados:
```
codigo,nombre,apellidos,departamento,salario,fechaAlta,categoria,situacion
00001,JUAN,GARCIA LOPEZ,INFORMATICA,37500.00,2020-01-15,SENIOR,ACTIVO
                                    ^^^^^^^^ (35000 + 2500)
```

**Equivalente COBOL:**
```cobol
SELECT SALIDA ASSIGN TO ...
ORGANIZATION IS SEQUENTIAL
ACCESS MODE IS SEQUENTIAL
```

---

## 🔄 Lógica del Matching 1:1

### En COBOL (PB0EC319):
```cobol
2000-CRUCE.
    EVALUATE TRUE
        WHEN CLAVE-MAESTRO = CODIGO-SUBIDAS
            * Incrementar salario
            ADD IMPORTE-SUBIDA TO SALARIO-MAESTRO
            WRITE REGISTRO-SALIDA FROM REGISTRO-MAESTRO
            
        WHEN CLAVE-MAESTRO < CODIGO-SUBIDAS
            * Mantener registro sin cambios
            WRITE REGISTRO-SALIDA FROM REGISTRO-MAESTRO
            
        WHEN CLAVE-MAESTRO > CODIGO-SUBIDAS
            * Inconsistencia: subida sin empleado
            DISPLAY "ERROR: Codigo " CODIGO-SUBIDAS " no existe"
    END-EVALUATE.
```

### En Java Spring Batch:
```java
@Override
public Empleado process(Empleado empleado) {
    // Buscar subida para este empleado
    Subida subida = subidasService.buscarSubida(empleado.getCodigo());
    
    if (subida != null) {
        // MATCHING: aplicar incremento
        BigDecimal nuevoSalario = empleado.getSalario().add(subida.getImporte());
        empleado.setSalario(nuevoSalario);
    }
    
    // Devolver empleado (con o sin modificación)
    return empleado;
}
```

---

## 🏗️ Arquitectura del Proyecto

```
incremento-salario-batch/
│
├── src/main/java/com/banco/batch/
│   ├── IncrementoSalarioBatchApplication.java  ← Main (punto de entrada)
│   │
│   ├── model/
│   │   ├── Empleado.java          ← Layout del MAESTRO
│   │   └── Subida.java            ← Layout de SUBIDAS
│   │
│   ├── reader/
│   │   └── MaestroEmpleadosReader.java  ← Lee el maestro (OPEN INPUT MAESTRO)
│   │
│   ├── processor/
│   │   └── IncrementoSalarioProcessor.java  ← Lógica del CRUCE
│   │
│   ├── writer/
│   │   └── SalidaEmpleadosWriter.java  ← Escribe SALIDA (WRITE REGISTRO-SALIDA)
│   │
│   ├── service/
│   │   └── SubidasService.java    ← Carga SUBIDAS en memoria
│   │
│   ├── listener/
│   │   └── IncrementoSalarioListener.java  ← Informe de ejecución
│   │
│   └── config/
│       └── IncrementoSalarioBatchConfig.java  ← Configuración del JOB
│
├── src/main/resources/
│   ├── data/
│   │   ├── maestro-empleados.csv  ← Fichero MAESTRO
│   │   └── subidas.csv            ← Fichero SUBIDAS
│   └── application.properties
│
├── output/
│   └── maestro-empleados-actualizado.csv  ← Fichero SALIDA (generado)
│
├── pom.xml
└── README.md
```

---

## 📊 Comparación Mainframe vs Spring Batch

| Concepto               | Mainframe (COBOL/JCL)                  | Spring Batch (Java)                    |
|------------------------|----------------------------------------|----------------------------------------|
| **Job**                | JCL (INCSAL.JCL)                       | `@Bean Job incrementoSalarioJob()`     |
| **Step**               | EXEC PGM=PB0EC319                      | `@Bean Step incrementoSalarioStep()`   |
| **Fichero entrada 1**  | DD MAESTRO                             | `FlatFileItemReader<Empleado>`         |
| **Fichero entrada 2**  | DD SUBIDAS                             | `SubidasService.cargarSubidas()`       |
| **Fichero salida**     | DD SALIDA                              | `FlatFileItemWriter<Empleado>`         |
| **Lógica del proceso** | Sección 2000-CRUCE                     | `IncrementoSalarioProcessor.process()` |
| **Contadores**         | CTR-LEIDOS-MAESTRO, CTR-LEIDOS-SUBIDAS | Variables estáticas en Processor       |
| **Informe final**      | DISPLAY al final                       | `IncrementoSalarioListener.afterJob()` |
| **Metadatos**          | SMF records                            | Tablas BATCH_* en H2                   |
| **Checkpoint**         | Manual con COMMIT                      | Automático cada chunk (50 registros)   |

---

## 🚀 Cómo ejecutar

### Prerrequisitos
- Java 17+
- Maven 3.6+

### Ejecución

```bash
cd incremento-salario-batch
mvn clean package
mvn spring-boot:run
```

### Salida esperada

```
================================================================================
APLICACION BATCH - INCREMENTO DE SALARIOS
Migración del proceso mainframe PB0EC319
================================================================================
ARQUITECTURA:
  ENTRADA 1: Maestro de empleados (maestro-empleados.csv)
  ENTRADA 2: Fichero de subidas (subidas.csv)
  SALIDA:    Maestro actualizado (maestro-empleados-actualizado.csv)
================================================================================

>>> CARGANDO FICHERO DE SUBIDAS...
>>> SUBIDAS CARGADAS: 13

================================================================================
COMIENZA EL PROGRAMA PB0EC319 - INCREMENTO DE SALARIOS
================================================================================
HOY ES: 2025-02-15
SON LAS: 14:30:00
================================================================================

>>> PROCESADOS: 50 empleados | ACTUALIZADOS: 12 salarios

   → Empleado 00001 (JUAN GARCIA LOPEZ): 35000.00 → 37500.00 (+2500.00) [REVISION ANUAL]
   → Empleado 00003 (CARLOS FERNANDEZ GIL): 28000.00 → 31000.00 (+3000.00) [PROMOCION]
   → Empleado 00004 (ANA RODRIGUEZ DIAZ): 42000.00 → 43500.00 (+1500.00) [REVISION ANUAL]
   ...

================================================================================
*** EJECUCION OK ***

ESTADISTICAS DE EJECUCION:
--------------------------------------------------------------------------------
LEIDOS MAESTRO                                          25
LEIDOS SUBIDAS                                          13
GRABADOS                                                25
SALARIOS ACTUALIZADOS                                   12
EMPLEADOS SIN SUBIDA                                    13

VALIDACIONES:
--------------------------------------------------------------------------------
*** ATENCION: Posibles inconsistencias detectadas ***
Hay 1 subidas que no se aplicaron (empleados no encontrados en maestro)

TIEMPOS DE EJECUCION:
--------------------------------------------------------------------------------
INICIO:   14:30:00
FIN:      14:30:01
DURACION: 234 ms
================================================================================
FIN DEL PROGRAMA PB0EC319
================================================================================
```

---

## 🔍 Detalles de implementación

### 1. Reader (Lectura del maestro)
```java
@Bean
public FlatFileItemReader<Empleado> empleadoReader() {
    // Lee el CSV línea por línea
    // Convierte cada línea a un objeto Empleado
    // Equivalente a: READ MAESTRO INTO REGISTRO-MAESTRO
}
```

### 2. Processor (Lógica del matching)
```java
@Override
public Empleado process(Empleado empleado) {
    Subida subida = subidasService.buscarSubida(empleado.getCodigo());
    
    if (subida != null) {
        // MATCHING 1:1 encontrado
        empleado.setSalario(empleado.getSalario().add(subida.getImporte()));
    }
    
    return empleado;
}
```

**Ventajas sobre COBOL:**
- ✅ No hace falta programar el matching manualmente
- ✅ Uso de Map (HashMap) para lookup O(1) en lugar de lectura secuencial
- ✅ Más eficiente: no hace falta leer SUBIDAS múltiples veces

### 3. Writer (Escritura del resultado)
```java
@Bean
public FlatFileItemWriter<Empleado> empleadoWriter() {
    // Escribe los empleados actualizados al fichero de salida
    // Equivalente a: WRITE REGISTRO-SALIDA
}
```

### 4. Chunk processing
```java
.<Empleado, Empleado>chunk(50, transactionManager)
```

**Equivalente COBOL:**
```cobol
PERFORM 2000-CRUCE
    UNTIL END-OF-FILE-MAESTRO
    
* Cada X registros podría hacer COMMIT (checkpoint)
```

**Ventajas:**
- ✅ Gestión automática de transacciones
- ✅ Si falla en el registro 120, puede retomar desde el registro 100 (último chunk)
- ✅ Memoria controlada (solo 50 registros en memoria a la vez)

---

## 📈 Ventajas de la migración

### Comparado con COBOL/Mainframe:

| Aspecto            | Mainframe                            | Spring Batch                                          |
|--------------------|--------------------------------------|-------------------------------------------------------|
| **Performance**    | Lectura secuencial de ambos ficheros | Fichero pequeño (SUBIDAS) en memoria → Lookup O(1)    |
| **Escalabilidad**  | Un solo thread                       | Multi-threading opcional                              |
| **Mantenibilidad** | COBOL (lenguaje legacy)              | Java (estándar actual)                                |
| **Testing**        | Complejo (requiere entorno mainframe)| Unit tests + Integration tests fáciles                |
| **Monitorización** | SMF + herramientas propietarias      | Spring Batch Admin + Métricas estándar                |
| **Coste**          | MIPS (muy caro)                      | Infraestructura cloud o on-premise estándar           |
| **Trazabilidad**   | Logs en SYSOUT                       | Base de datos con tablas BATCH_* + logs estructurados |
| **Restart**        | Manual (checkpoint)                  | Automático desde último chunk                         |

---

## 🧪 Testing

### Casos de prueba incluidos:

1. **Empleado con subida** → Salario se incrementa
2. **Empleado sin subida** → Registro se mantiene igual
3. **Subida sin empleado** (código 00099) → Se detecta inconsistencia
4. **Validación de contadores** → LEIDOS = GRABADOS

### Para ejecutar tests:
```bash
mvn test
```

---

## 📊 Métricas y monitorización

### Acceder a la consola H2:
1. Ejecutar la aplicación
2. Abrir: http://localhost:8080/h2-console
3. Conectar con:
   - JDBC URL: `jdbc:h2:mem:batchdb`
   - User: `sa`
   - Password: (vacío)

### Consultas útiles:

```sql
-- Ver ejecución del job
SELECT * FROM BATCH_JOB_EXECUTION;

-- Ver métricas del step
SELECT 
    step_name,
    status,
    read_count,
    write_count,
    commit_count,
    start_time,
    end_time
FROM BATCH_STEP_EXECUTION;

-- Ver parámetros de ejecución
SELECT * FROM BATCH_JOB_EXECUTION_PARAMS;
```

---

## 💼 Explicación

### Pregunta: "¿Cómo migrarías un proceso batch de mainframe a Java?"

**Respuesta:**

> Migrar el proceso PB0EC319 que hace un cruce de ficheros 
> para incrementar salarios de empleados. El proceso original en COBOL leía 
> dos ficheros secuenciales y hacía un matching 1:1.
> 
> Se migra a Spring Batch siguiendo esta estrategia:
> 
> 1. **Análisis del COBOL**: Identificar que es un proceso Reader-Processor-Writer 
>    con matching por código de empleado.
> 
> 2. **Optimización**: En lugar de leer ambos ficheros en paralelo (como en COBOL), 
>    cargar el fichero pequeño (SUBIDAS) en un HashMap. Esto convierte un proceso 
>    O(n*m) en O(n), mucho más eficiente.
> 
> 3. **Chunk processing**: Configurar chunks de 50 registros para control de memoria 
>    y habilitar restart automático.
> 
> 4. **Trazabilidad**: Implementar listeners que generan el mismo informe que el 
>    COBOL (contadores, tiempos, inconsistencias).
> 
> 5. **Testing**: Crear tests unitarios e integración, algo imposible en mainframe.
> 
> El resultado: mismo comportamiento funcional, 10x más rápido, mucho 
> más barato (sin MIPS), y mucho más fácil de mantener.

---

## 🎓 Conceptos clave 

### 1. Matching 1:1
- Cruce de dos ficheros ordenados por clave
- En mainframe: lectura secuencial de ambos
- En Spring Batch: un fichero en memoria (Map)

### 2. Chunk-oriented processing
- Equivalente a los checkpoints en COBOL
- Transacción por cada N registros
- Restart automático desde último chunk

### 3. Gestión de inconsistencias
- Detectar registros en SUBIDAS sin empleado en MAESTRO
- En COBOL: WHEN CLAVE-MAESTRO > CODIGO-SUBIDAS
- En Java: comparar claves procesadas vs claves en el Map

### 4. Contadores y trazabilidad
- CTR-LEIDOS-MAESTRO → `totalEmpleadosLeidos`
- CTR-LEIDOS-SUBIDAS → `subidasService.getTotalSubidasLeidas()`
- CTR-GRABADOS → `empleadoWriter` statistics

---

## 📚 Referencias

- Repositorio original COBOL: https://github.com/eetxlek/incremento-salario-empleados
- [Documentación Spring Batch](https://docs.spring.io/spring-batch/docs/current/reference/html/)

---

## ✅ Checklist de migración mainframe → Spring Batch

- [x] Análisis del programa COBOL original
- [x] Identificación de ficheros de entrada/salida
- [x] Mapeo de layouts COBOL a clases Java
- [x] Implementación del Reader (SELECT...OPEN...READ)
- [x] Implementación del Processor (lógica del programa)
- [x] Implementación del Writer (WRITE)
- [x] Replicación de contadores y estadísticas
- [x] Gestión de errores e inconsistencias
- [x] Informe de ejecución equivalente
- [x] Datos de prueba
- [x] Documentación

---

**Autor:** Migración del programa PB0EC319 (ESTIBALIZ)  
**Fecha:** Febrero 2025  
**Stack:** Java 17 + Spring Batch + Maven
