package com.banco.batch.service;

import com.banco.batch.model.Subida;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Service;

import jakarta.annotation.PostConstruct;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;

/**
 * Servicio que carga el fichero SUBIDAS en memoria
 * 
 * Equivalente COBOL:
 * SELECT SUBIDAS ASSIGN TO ...
 * ORGANIZATION IS SEQUENTIAL
 * ACCESS MODE IS SEQUENTIAL
 * 
 * En COBOL harías un matching 1:1 leyendo ambos ficheros en paralelo.
 * En Java Spring Batch, es más eficiente cargar las subidas en un Map
 * y hacer lookup directo por código.
 */
@Service
public class SubidasService {
    
    // Map que simula tener el fichero SUBIDAS indexado en memoria
    // Clave: código empleado, Valor: subida
    private Map<String, Subida> mapaSubidas = new HashMap<>();
    
    // Contadores (equivalente a los CTR- en COBOL)
    private int totalSubidasLeidas = 0;
    
    /**
     * Carga el fichero SUBIDAS al iniciar la aplicación
     * Equivalente a OPEN INPUT SUBIDAS + READ SUBIDAS en COBOL
     */
    @PostConstruct
    public void cargarSubidas() {
        System.out.println("\n>>> CARGANDO FICHERO DE SUBIDAS...");
        
        try (BufferedReader br = new BufferedReader(
                new InputStreamReader(
                    new ClassPathResource("data/subidas.csv").getInputStream()))) {
            
            String line;
            boolean primeraLinea = true;
            
            while ((line = br.readLine()) != null) {
                // Saltar cabecera
                if (primeraLinea) {
                    primeraLinea = false;
                    continue;
                }
                
                String[] campos = line.split(",");
                String codigo = campos[0].trim();
                BigDecimal importe = new BigDecimal(campos[1].trim());
                String motivo = campos.length > 2 ? campos[2].trim() : "REVISION ANUAL";
                
                Subida subida = new Subida(codigo, importe, motivo);
                mapaSubidas.put(codigo, subida);
                totalSubidasLeidas++;
            }
            
            System.out.println(">>> SUBIDAS CARGADAS: " + totalSubidasLeidas);
            
        } catch (Exception e) {
            System.err.println("*** ERROR AL CARGAR FICHERO SUBIDAS: " + e.getMessage());
            throw new RuntimeException("Error fatal cargando subidas", e);
        }
    }
    
    /**
     * Busca si existe una subida para un empleado
     * Equivalente a: IF CLAVE-MAESTRO = CODIGO-SUBIDAS
     */
    public Subida buscarSubida(String codigoEmpleado) {
        return mapaSubidas.get(codigoEmpleado);
    }
    
    /**
     * Verifica si existe subida para un código
     */
    public boolean tieneSubida(String codigoEmpleado) {
        return mapaSubidas.containsKey(codigoEmpleado);
    }
    
    /**
     * Obtiene el total de subidas leídas
     * Equivalente a: CTR-LEIDOS-SUBIDAS
     */
    public int getTotalSubidasLeidas() {
        return totalSubidasLeidas;
    }
    
    /**
     * Obtiene todas las subidas (para validación de inconsistencias)
     */
    public Map<String, Subida> getMapaSubidas() {
        return new HashMap<>(mapaSubidas);
    }
}
