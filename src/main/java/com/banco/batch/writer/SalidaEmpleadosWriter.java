package com.banco.batch.writer;

import com.banco.batch.model.Empleado;
import org.springframework.batch.item.file.FlatFileItemWriter;
import org.springframework.batch.item.file.transform.BeanWrapperFieldExtractor;
import org.springframework.batch.item.file.transform.DelimitedLineAggregator;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.io.FileSystemResource;

/**
 * WRITER para el fichero de SALIDA
 * 
 * Equivalente COBOL:
 * SELECT SALIDA ASSIGN TO ...
 * ORGANIZATION IS SEQUENTIAL
 * ACCESS MODE IS SEQUENTIAL
 * 
 * WRITE REGISTRO-SALIDA
 */
@Configuration
public class SalidaEmpleadosWriter {
    
    @Bean
    public FlatFileItemWriter<Empleado> empleadoWriter() {
        FlatFileItemWriter<Empleado> writer = new FlatFileItemWriter<>();
        writer.setName("empleadoWriter");
        
        // Fichero de salida (equivalente al DD SALIDA del JCL)
        writer.setResource(new FileSystemResource("output/maestro-empleados-actualizado.csv"));
        
        // Configurar el agregador de líneas
        DelimitedLineAggregator<Empleado> lineAggregator = new DelimitedLineAggregator<>();
        lineAggregator.setDelimiter(",");
        
        // Extractor de campos (equivalente a mover campos en COBOL)
        BeanWrapperFieldExtractor<Empleado> fieldExtractor = new BeanWrapperFieldExtractor<>();
        fieldExtractor.setNames(new String[] {
            "codigo", "nombre", "apellidos", "departamento",
            "salario", "fechaAlta", "categoria", "situacion"
        });
        
        lineAggregator.setFieldExtractor(fieldExtractor);
        writer.setLineAggregator(lineAggregator);
        
        // Escribir cabecera en el fichero de salida
        writer.setHeaderCallback(writerCallback -> {
            writerCallback.write("codigo,nombre,apellidos,departamento,salario,fechaAlta,categoria,situacion");
        });
        
        return writer;
    }
}
