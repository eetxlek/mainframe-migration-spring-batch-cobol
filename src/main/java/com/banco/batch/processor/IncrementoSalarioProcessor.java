package com.banco.batch.processor;

import com.banco.batch.model.Empleado;
import com.banco.batch.model.Subida;
import com.banco.batch.service.SubidasService;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;

/**
 * PROCESSOR que implementa la lógica del CRUCE (MATCHING 1:1)
 * 
 * Equivalente COBOL (sección 2000-CRUCE del programa PB0EC319):
 * 
 * EVALUATE TRUE
 *    WHEN CLAVE-MAESTRO = CODIGO-SUBIDAS
 *       → Incrementar salario
 *    WHEN CLAVE-MAESTRO < CODIGO-SUBIDAS
 *       → Mantener registro
 *    WHEN CLAVE-MAESTRO > CODIGO-SUBIDAS
 *       → Inconsistencia (subida sin empleado)
 * END-EVALUATE
 */
@Component
public class IncrementoSalarioProcessor implements ItemProcessor<Empleado, Empleado> {
    
    @Autowired
    private SubidasService subidasService;
    
    // Contadores (equivalente a los CTR- en COBOL)
    private static int totalEmpleadosLeidos = 0;
    private static int totalSalariosActualizados = 0;
    private static int totalEmpleadosSinSubida = 0;
    
    @Override
    public Empleado process(Empleado empleado) throws Exception {
        totalEmpleadosLeidos++;
        
        // Buscar si existe subida para este empleado
        Subida subida = subidasService.buscarSubida(empleado.getCodigo());
        
        if (subida != null) {
            // MATCHING: CLAVE-MAESTRO = CODIGO-SUBIDAS
            // → Aplicar incremento salarial
            
            BigDecimal salarioAnterior = empleado.getSalario();
            BigDecimal nuevoSalario = salarioAnterior.add(subida.getImporte());
            empleado.setSalario(nuevoSalario);
            
            totalSalariosActualizados++;
            
            if (totalSalariosActualizados % 50 == 0) {
                System.out.println(String.format(
                    ">>> PROCESADOS: %d empleados | ACTUALIZADOS: %d salarios",
                    totalEmpleadosLeidos, totalSalariosActualizados
                ));
            }
            
            // Log detallado (opcional, como el DISPLAY del COBOL)
            logActualizacion(empleado, salarioAnterior, nuevoSalario, subida);
            
        } else {
            // NO MATCHING: No hay subida para este empleado
            // → Mantener registro sin cambios
            totalEmpleadosSinSubida++;
        }
        
        // Siempre devolvemos el empleado (con o sin modificación)
        // Equivalente a WRITE REGISTRO-SALIDA en COBOL
        return empleado;
    }
    
    /**
     * Log de la actualización (equivalente al DISPLAY en COBOL)
     */
    private void logActualizacion(Empleado emp, BigDecimal anterior, 
                                  BigDecimal nuevo, Subida subida) {
        // Solo loggeamos algunos para no saturar (en COBOL también se hace así)
        if (totalSalariosActualizados <= 5 || totalSalariosActualizados % 100 == 0) {
            System.out.println(String.format(
                "   → Empleado %s (%s %s): %s → %s (+%s) [%s]",
                emp.getCodigo(),
                emp.getNombre(),
                emp.getApellidos(),
                anterior,
                nuevo,
                subida.getImporte(),
                subida.getMotivo()
            ));
        }
    }
    
    /**
     * Obtiene estadísticas (para el informe final)
     */
    public static int getTotalEmpleadosLeidos() {
        return totalEmpleadosLeidos;
    }
    
    public static int getTotalSalariosActualizados() {
        return totalSalariosActualizados;
    }
    
    public static int getTotalEmpleadosSinSubida() {
        return totalEmpleadosSinSubida;
    }
    
    /**
     * Resetea contadores (para poder ejecutar múltiples veces en tests)
     */
    public static void resetContadores() {
        totalEmpleadosLeidos = 0;
        totalSalariosActualizados = 0;
        totalEmpleadosSinSubida = 0;
    }
}
