package com.banco.batch.listener;

import com.banco.batch.model.Subida;
import com.banco.batch.processor.IncrementoSalarioProcessor;
import com.banco.batch.service.SubidasService;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobExecutionListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * LISTENER que genera el informe de ejecución
 * 
 * Equivalente COBOL:
 * - ACCEPT WS-FECHA FROM DATE
 * - ACCEPT WS-HORA FROM TIME
 * - DISPLAY "COMIENZA EL PROGRAMA PB0EC319"
 * - DISPLAY "HOY ES: " WS-FECHA-EDITADA
 * - DISPLAY "SON LAS: " WS-HORA-EDITADA
 * - [... al final ...]
 * - DISPLAY "*** EJECUCION OK ***"
 * - DISPLAY "LEIDOS MAESTRO " CTR-LEIDOS-MAESTRO
 * - DISPLAY "LEIDOS SUBIDAS " CTR-LEIDOS-SUBIDAS
 * - DISPLAY "GRABADOS " CTR-GRABADOS
 * - DISPLAY "FIN DEL PROGRAMA PB0EC319"
 */
@Component
public class IncrementoSalarioListener implements JobExecutionListener {
    
    @Autowired
    private SubidasService subidasService;
    
    private LocalDateTime fechaHoraInicio;
    private static final DateTimeFormatter FORMATO_FECHA = DateTimeFormatter.ofPattern("yyyy-MM-dd");
    private static final DateTimeFormatter FORMATO_HORA = DateTimeFormatter.ofPattern("HH:mm:ss");
    
    @Override
    public void beforeJob(JobExecution jobExecution) {
        fechaHoraInicio = LocalDateTime.now();
        
        // Resetear contadores del processor
        IncrementoSalarioProcessor.resetContadores();
        
        // Cabecera del informe (equivalente al inicio del programa COBOL)
        System.out.println("\n" + "=".repeat(80));
        System.out.println("COMIENZA EL PROGRAMA PB0EC319 - INCREMENTO DE SALARIOS");
        System.out.println("=".repeat(80));
        System.out.println("HOY ES: " + fechaHoraInicio.format(FORMATO_FECHA));
        System.out.println("SON LAS: " + fechaHoraInicio.format(FORMATO_HORA));
        System.out.println("=".repeat(80));
        System.out.println();
    }
    
    @Override
    public void afterJob(JobExecution jobExecution) {
        LocalDateTime fechaHoraFin = LocalDateTime.now();
        Duration duracion = Duration.between(fechaHoraInicio, fechaHoraFin);
        
        // Obtener contadores
        int leidosMaestro = IncrementoSalarioProcessor.getTotalEmpleadosLeidos();
        int leidosSubidas = subidasService.getTotalSubidasLeidas();
        int salariosActualizados = IncrementoSalarioProcessor.getTotalSalariosActualizados();
        int empleadosSinSubida = IncrementoSalarioProcessor.getTotalEmpleadosSinSubida();
        
        // Calcular inconsistencias (subidas sin empleado en el maestro)
        Set<String> codigosConSubida = subidasService.getMapaSubidas().keySet();
        Set<String> codigosProcesados = new HashSet<>();
        
        // Identificar subidas sin empleado (equivalente a CLAVE-MAESTRO > CODIGO-SUBIDAS)
        int inconsistencias = 0;
        for (String codigo : codigosConSubida) {
            // Si la subida no se aplicó a nadie, es una inconsistencia
            // En el COBOL esto se detectaba cuando CODE-SUB < CLAVE-MAESTRO
        }
        
        // Informe final (equivalente al final del programa COBOL)
        System.out.println("\n" + "=".repeat(80));
        
        if (jobExecution.getStatus().toString().equals("COMPLETED")) {
            System.out.println("*** EJECUCION OK ***");
        } else {
            System.out.println("*** A T E N C I O N  ***");
            System.out.println("*** ERRORES EN LA EJECUCION ***");
        }
        
        System.out.println();
        System.out.println("ESTADISTICAS DE EJECUCION:");
        System.out.println("-".repeat(80));
        System.out.println(String.format("%-40s %10d", "LEIDOS MAESTRO", leidosMaestro));
        System.out.println(String.format("%-40s %10d", "LEIDOS SUBIDAS", leidosSubidas));
        System.out.println(String.format("%-40s %10d", "GRABADOS", leidosMaestro));
        System.out.println(String.format("%-40s %10d", "SALARIOS ACTUALIZADOS", salariosActualizados));
        System.out.println(String.format("%-40s %10d", "EMPLEADOS SIN SUBIDA", empleadosSinSubida));
        
        // Validación de consistencia
        System.out.println();
        System.out.println("VALIDACIONES:");
        System.out.println("-".repeat(80));
        
        // Detectar subidas sin empleado en maestro
        detectarInconsistencias();
        
        // Información temporal
        System.out.println();
        System.out.println("TIEMPOS DE EJECUCION:");
        System.out.println("-".repeat(80));
        System.out.println("INICIO:   " + fechaHoraInicio.format(FORMATO_HORA));
        System.out.println("FIN:      " + fechaHoraFin.format(FORMATO_HORA));
        System.out.println("DURACION: " + duracion.toMillis() + " ms");
        
        System.out.println("=".repeat(80));
        System.out.println("FIN DEL PROGRAMA PB0EC319");
        System.out.println("=".repeat(80) + "\n");
    }
    
    /**
     * Detecta inconsistencias: subidas sin empleado en el maestro
     * Equivalente COBOL: WHEN CLAVE-MAESTRO > CODIGO-SUBIDAS
     */
    private void detectarInconsistencias() {
        Map<String, Subida> subidas = subidasService.getMapaSubidas();
        int totalEmpleados = IncrementoSalarioProcessor.getTotalEmpleadosLeidos();
        int salariosActualizados = IncrementoSalarioProcessor.getTotalSalariosActualizados();
        
        // Si hay menos actualizaciones que subidas, hay inconsistencias
        int posiblesInconsistencias = subidas.size() - salariosActualizados;
        
        if (posiblesInconsistencias > 0) {
            System.out.println("*** ATENCION: Posibles inconsistencias detectadas ***");
            System.out.println(String.format(
                "Hay %d subidas que no se aplicaron (empleados no encontrados en maestro)",
                posiblesInconsistencias
            ));
        } else {
            System.out.println("✓ No se detectaron inconsistencias");
            System.out.println("✓ Todas las subidas se aplicaron correctamente");
        }
    }
}
