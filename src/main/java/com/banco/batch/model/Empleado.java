package com.banco.batch.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

/**
 * Representa un registro del fichero MAESTRO de empleados
 * Equivalente al layout COBOL del MAESTRO
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class Empleado {
    
    // CLAVE (X(5)) - Código único del empleado
    private String codigo;
    
    // Campos del empleado
    private String nombre;
    private String apellidos;
    private String departamento;
    
    // Salario actual (equivalente a S9(9)V99 PACKED-DECIMAL)
    private BigDecimal salario;
    
    // Fecha de alta
    private String fechaAlta;
    
    // Campos adicionales (FILLER en COBOL)
    private String categoria;
    private String situacion;
    
    /**
     * Constructor para crear un empleado desde una línea CSV
     */
    public static Empleado fromCsvLine(String line) {
        String[] fields = line.split(",");
        Empleado emp = new Empleado();
        emp.setCodigo(fields[0].trim());
        emp.setNombre(fields[1].trim());
        emp.setApellidos(fields[2].trim());
        emp.setDepartamento(fields[3].trim());
        emp.setSalario(new BigDecimal(fields[4].trim()));
        emp.setFechaAlta(fields[5].trim());
        emp.setCategoria(fields[6].trim());
        emp.setSituacion(fields[7].trim());
        return emp;
    }
    
    /**
     * Convierte el empleado a formato CSV para escribir
     */
    public String toCsvLine() {
        return String.format("%s,%s,%s,%s,%s,%s,%s,%s",
            codigo, nombre, apellidos, departamento, 
            salario, fechaAlta, categoria, situacion);
    }
}
