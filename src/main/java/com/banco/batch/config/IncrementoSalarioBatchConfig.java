package com.banco.batch.config;

import com.banco.batch.listener.IncrementoSalarioListener;
import com.banco.batch.model.Empleado;
import com.banco.batch.processor.IncrementoSalarioProcessor;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.launch.support.RunIdIncrementer;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.item.file.FlatFileItemReader;
import org.springframework.batch.item.file.FlatFileItemWriter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.PlatformTransactionManager;

/**
 * CONFIGURACIÓN DEL JOB DE INCREMENTO DE SALARIOS
 * 
 * Equivalente al JCL INCSAL.JCL:
 * 
 * //INCSAL     JOB (ACCT),'INCREMENTO SALARIO',CLASS=A,MSGCLASS=X
 * //STEP1      EXEC PGM=PB0EC319
 * //MAESTRO    DD  DSN=USER.MAESTRO.EMPLEADOS,DISP=SHR
 * //SUBIDAS    DD  DSN=USER.SUBIDAS,DISP=SHR
 * //SALIDA     DD  DSN=USER.MAESTRO.SALIDA,DISP=(NEW,CATLG,DELETE)
 * 
 * Este Job implementa el proceso batch de cruce de ficheros:
 * - Lee el maestro de empleados (MAESTRO)
 * - Busca subidas salariales (SUBIDAS)
 * - Genera fichero de salida actualizado (SALIDA)
 */
@Configuration
public class IncrementoSalarioBatchConfig {
    
    @Autowired
    private IncrementoSalarioProcessor incrementoSalarioProcessor;
    
    @Autowired
    private IncrementoSalarioListener incrementoSalarioListener;
    
    @Autowired
    private FlatFileItemReader<Empleado> empleadoReader;
    
    @Autowired
    private FlatFileItemWriter<Empleado> empleadoWriter;
    
    /**
     * STEP - Proceso de incremento de salarios
     * 
     * Equivalente al STEP1 del JCL que ejecuta el programa PB0EC319
     */
    @Bean
    public Step incrementoSalarioStep(JobRepository jobRepository,
                                      PlatformTransactionManager transactionManager) {
        return new StepBuilder("incrementoSalarioStep", jobRepository)
                .<Empleado, Empleado>chunk(50, transactionManager)  // Procesa de 50 en 50
                .reader(empleadoReader)
                .processor(incrementoSalarioProcessor)
                .writer(empleadoWriter)
                .build();
    }
    
    /**
     * JOB - Incremento de Salarios
     * 
     * Equivalente al JOB INCSAL del mainframe
     */
    @Bean
    public Job incrementoSalarioJob(JobRepository jobRepository,
                                    Step incrementoSalarioStep) {
        return new JobBuilder("incrementoSalarioJob", jobRepository)
                .incrementer(new RunIdIncrementer())
                .listener(incrementoSalarioListener)
                .start(incrementoSalarioStep)
                .build();
    }
}
