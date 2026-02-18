package com.banco.batch.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

/**
 * Representa un registro del fichero SUBIDAS
 * Equivalente al layout COBOL del fichero de subidas salariales
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class Subida {
    
    // CODIGO (X(5)) - Código del empleado que recibe subida
    private String codigo;
    
    // SUBIDA (S9(9)V99 PACKED-DECIMAL) - Importe del incremento
    private BigDecimal importe;
    
    // Motivo de la subida (campo adicional)
    private String motivo;
    
    /**
     * Constructor para crear una subida desde una línea CSV
     */
    public static Subida fromCsvLine(String line) {
        String[] fields = line.split(",");
        Subida subida = new Subida();
        subida.setCodigo(fields[0].trim());
        subida.setImporte(new BigDecimal(fields[1].trim()));
        subida.setMotivo(fields.length > 2 ? fields[2].trim() : "REVISION ANUAL");
        return subida;
    }
}
