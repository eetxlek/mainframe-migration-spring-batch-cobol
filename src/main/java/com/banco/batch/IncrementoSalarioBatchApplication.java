package com.banco.batch;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * APLICACIÓN PRINCIPAL - Incremento de Salarios Batch
 * 
 * Equivalente al programa mainframe PB0EC319
 * 
 * PROCESO:
 * 1. Lee el maestro de empleados (MAESTRO KSDS)
 * 2. Carga el fichero de subidas en memoria (SUBIDAS PSD)
 * 3. Hace matching 1:1 por código de empleado
 * 4. Genera fichero de salida actualizado (SALIDA PSD)
 * 5. Emite informe de ejecución con contadores
 * 
 * Migración de:
 * - JCL: INCSAL.JCL
 * - COBOL: PB0EC319 (Cruce.cbl)
 */
@SpringBootApplication
public class IncrementoSalarioBatchApplication {
    
    public static void main(String[] args) {
        System.out.println("\n" + "=".repeat(80));
        System.out.println("APLICACION BATCH - INCREMENTO DE SALARIOS");
        System.out.println("Migración del proceso mainframe PB0EC319");
        System.out.println("=".repeat(80));
        System.out.println("ARQUITECTURA:");
        System.out.println("  ENTRADA 1: Maestro de empleados (maestro-empleados.csv)");
        System.out.println("  ENTRADA 2: Fichero de subidas (subidas.csv)");
        System.out.println("  SALIDA:    Maestro actualizado (maestro-empleados-actualizado.csv)");
        System.out.println("=".repeat(80) + "\n");
        
        SpringApplication.run(IncrementoSalarioBatchApplication.class, args);
    }
}
