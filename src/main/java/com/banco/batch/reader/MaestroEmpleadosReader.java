package com.banco.batch.reader;

import com.banco.batch.model.Empleado;
import org.springframework.batch.item.file.FlatFileItemReader;
import org.springframework.batch.item.file.mapping.DefaultLineMapper;
import org.springframework.batch.item.file.transform.DelimitedLineTokenizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.io.ClassPathResource;

/**
 * READER para el fichero MAESTRO de empleados
 * 
 * Equivalente COBOL:
 * SELECT MAESTRO ASSIGN TO ...
 * ORGANIZATION IS INDEXED
 * ACCESS MODE IS SEQUENTIAL
 * RECORD KEY IS CLAVE
 */
@Configuration
public class MaestroEmpleadosReader {
    
    @Bean
    public FlatFileItemReader<Empleado> empleadoReader() {
        FlatFileItemReader<Empleado> reader = new FlatFileItemReader<>();
        reader.setName("empleadoReader");
        reader.setResource(new ClassPathResource("data/maestro-empleados.csv"));
        reader.setLinesToSkip(1); // Saltar cabecera
        
        // Configuración del mapper (equivalente a la COPY del COBOL)
        DefaultLineMapper<Empleado> lineMapper = new DefaultLineMapper<>();
        
        // Tokenizer - define cómo separar los campos
        DelimitedLineTokenizer tokenizer = new DelimitedLineTokenizer();
        tokenizer.setNames("codigo", "nombre", "apellidos", "departamento", 
                          "salario", "fechaAlta", "categoria", "situacion");
        tokenizer.setDelimiter(",");
        
        lineMapper.setLineTokenizer(tokenizer);
        
        // Field set mapper - convierte los tokens a objeto Empleado
        lineMapper.setFieldSetMapper(fieldSet -> {
            Empleado empleado = new Empleado();
            empleado.setCodigo(fieldSet.readString("codigo"));
            empleado.setNombre(fieldSet.readString("nombre"));
            empleado.setApellidos(fieldSet.readString("apellidos"));
            empleado.setDepartamento(fieldSet.readString("departamento"));
            empleado.setSalario(fieldSet.readBigDecimal("salario"));
            empleado.setFechaAlta(fieldSet.readString("fechaAlta"));
            empleado.setCategoria(fieldSet.readString("categoria"));
            empleado.setSituacion(fieldSet.readString("situacion"));
            return empleado;
        });
        
        reader.setLineMapper(lineMapper);
        
        return reader;
    }
}
