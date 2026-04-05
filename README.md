# 🏥 OdontoGest: Intelligent Clinical Management System with CDSS and RAG-based AI

**Final Degree Project (TFG)**  
**A Data-Driven Approach to Clinical Decision Support using Machine Learning, Explainable AI, and Salesforce-powered Retrieval-Augmented Generation**

---

## 📌 Abstract

This project presents the design, development, and evaluation of an intelligent clinical platform that integrates a **Dental Management System (OdontoGest)** with a **Clinical Decision Support System (CDSS)**. The platform is enhanced by **Machine Learning (ML)**, **Explainable Artificial Intelligence (XAI)**, and **Retrieval-Augmented Generation (RAG)** powered by **Salesforce Agentforce**.

The system addresses inefficiencies in traditional clinical workflows by introducing a **proactive, data-driven paradigm**, combining structured clinical data with unstructured knowledge sources to support both operational management and complex clinical decision-making.

---

## 🎯 Research Objectives

- Design and implement a modular clinical management system in **R Shiny**.
- Develop predictive models for inventory forecasting and diagnostic classification.
- Ensure interpretability through **SHAP-based Explainable AI (XAI)**.
- Implement a **RAG-based generative AI assistant** using **Salesforce Agentforce**.
- Evaluate system performance using quantitative (ML metrics) and qualitative (clinical usability) criteria.

---

## 🧪 Methodology

The project follows a structured and iterative methodology:

### 1. Data Collection & Preprocessing
- Clinical variables and operational datasets  
- Data cleaning, normalization, and transformation  

### 2. Model Development
- Regression and classification models  
- Hyperparameter tuning using cross-validation (caret)  

### 3. System Integration
- Integration into a modular **R Shiny** application  
- Decoupled architecture (UI / Server / ML modules)  

### 4. Evaluation
- Quantitative metrics (ML performance)  
- Qualitative validation (clinical interpretability and usability)  

---

## 🧩 System Architecture

The platform is composed of two main subsystems:

### 🦷 OdontoGest (Clinical Management System)

A web application developed in **R Shiny** providing:

- Patient management (EHR)  
- Appointment scheduling  
- Inventory management  
- Role-Based Access Control (RBAC)  
- Interactive dashboards  

---

### 🧠 Clinical Decision Support System (CDSS)

An intelligent layer that integrates:

- Predictive models  
- Diagnostic classification  
- Explainability mechanisms  
- Generative AI assistant (RAG) 

## 🔐 Security and Access Control

The system implements a **Role-Based Access Control (RBAC)** model with the following roles:

- Doctor  
- Hygienist  
- Receptionist  
- Administrator  
- Laboratory  
- Commercial  
- Patient  
- Visitor  

### Security Features

- Password hashing using `bcrypt`  
- Secure token generation with `sodium`  
- Controlled access to sensitive clinical data  

---

## 🚀 Installation and Usage

### Requirements

- R environment  
- Required packages  

### ▶️ Run locally

```r
install.packages(c(
  "shiny", "caret", "xgboost", "dplyr",
  "plotly", "fastshap", "bcrypt",
  "sodium", "pool", "RMariaDB", "emayili"
))

shiny::runApp("ClinicAppTFG")

```

---

## 🐳 Docker

```bash
docker build -t odontogest-app .
docker run -p 3838:3838 odontogest-app
```

## 📊 Contributions

- Integration of **Machine Learning, Explainable AI (XAI), and Retrieval-Augmented Generation (RAG)** in a healthcare environment  
- Practical implementation of a **Clinical Decision Support System (CDSS)**  
- Combination of **structured and unstructured data sources** for intelligent decision-making  
- Development of a **real-world applicable clinical platform**  

---

## 🔮 Future Work

- Integration with healthcare interoperability standards (**FHIR**)  
- Incorporation of advanced **Deep Learning models**  
- Radiographic image analysis using **Computer Vision techniques**  
- Development of **scalable and real-time RAG pipelines**  

---

## 🧠 Artificial Intelligence Architecture

### 1. Predictive Modeling (Machine Learning)
- **Inventory Forecasting:** Log-Linear Regression to predict monthly demand based on historical consumption and patient ratios.
- **Clinical Diagnosis:** **XGBoost** classifier for periodontal severity assessment using clinical biomarkers.
- **Explainability:** Integration of **SHAP values** to provide transparency, showing the weight of each clinical variable in the AI's diagnostic suggestion.

### 2. Generative AI & RAG (Salesforce Agentforce)
The system features an intelligent assistant named **"Dietencito"**, built on the **Salesforce** ecosystem to ensure enterprise-grade reliability and security.

*   **Technology Provider:** Salesforce (Agentforce / Data Cloud).
*   **RAG Mechanism:** The assistant retrieves context from a **Data Library** containing clinical guidelines, PDFs, and nutritional protocols before generating a response.
*   **Grounded Knowledge:** By using Salesforce's retrieval pipeline, the model minimizes hallucinations and provides advice strictly based on the provided clinical documentation.

---

## 🔄 RAG Pipeline (Powered by Salesforce)

The integration follows a sophisticated flow to ensure data accuracy:

1.  **Data Ingestion:** Clinical PDFs and protocols are ingested into the Salesforce Data Library.
2.  **Vector Search:** When a user queries the assistant, Salesforce's retrieval engine performs a semantic search.
3.  **Context Injection:** The most relevant "chunks" of information are injected into the LLM prompt.
4.  **Grounded Generation:** The agent generates a response that is contextually accurate and clinically relevant.

---

## ⚙️ Technology Stack

### Backend & Data Science
- **R 4.x:** Core language for logic and statistical modeling.
- **caret & xgboost:** Machine Learning training and optimization.
- **fastshap:** Model interpretability (XAI).

### Frontend & Integration
- **R Shiny:** Web framework for the clinical dashboard.
- **Salesforce Agentforce:** RAG-based AI Assistant and vector knowledge management.

### Database & Security
- **MySQL / PostgreSQL:** Relational data management.
- **bcrypt & sodium:** Password hashing and secure cryptographic tokens.

### DevOps
- **Docker:** Containerization for consistent deployment.
- **GitHub Actions:** CI/CD pipeline for automated testing and deployment.

---

## 📁 Repository Structure

```text
.
├── ClinicAppTFG/        # Shiny application (UI/Server modules)
├── modelos/             # ML training scripts and serialized models
├── data/                # Database schemas and synthetic datasets
├── docs/                # Architecture diagrams and RAG workflow
├── tests/               # Unit testing suite (testthat)
└── README.md

```

## 🤖 Implementación de IA Generativa: RAG & Salesforce Agentforce

La innovación disruptiva de este proyecto es el asistente inteligente **"Dietencito"**, que utiliza una arquitectura de **Generación Aumentada por Recuperación (RAG)**. A diferencia de un chat convencional, este sistema no "inventa" respuestas, sino que las fundamenta en evidencia científica cargada previamente en el sistema.

### 🏗️ El Ecosistema Salesforce Agentforce

Se ha seleccionado **Salesforce** como infraestructura para el RAG por su capacidad de manejar datos clínicos con alta seguridad y su motor de búsqueda vectorial nativo:

*   **Data Cloud:** Actúa como el "cerebro" del sistema. Aquí se almacenan y vectorizan los documentos no estructurados (PDFs de guías clínicas, protocolos de cirugía, recomendaciones nutricionales).
*   **Atlas Reasoning Engine:** Es el motor de razonamiento de Agentforce. Interpreta la intención del usuario, evalúa el contexto y decide qué documentos recuperar antes de generar la respuesta final.
*   **Einstein Trust Layer:** Capa de seguridad que anonimiza datos sensibles (PII) antes de enviarlos al LLM, garantizando que la clínica cumpla con las normativas de protección de datos de salud.

### 🔄 Flujo de Trabajo del RAG (Pipeline)

El proceso de respuesta sigue un pipeline riguroso para asegurar la veracidad (*grounding*):

1.  **Ingestión y Chunking:** Fragmentación de documentos para búsquedas precisas.
2.  **Vectorización (Embeddings):** Conversión de fragmentos en vectores semánticos.
3.  **Recuperación Semántica:** Búsqueda en la base de datos vectorial de los fragmentos con mayor similitud a la consulta.
4.  **Aumentación del Prompt:** Construcción de un prompt dinámico que incluye los fragmentos recuperados como única fuente de verdad.
5.  **Generación Grounded:** El LLM genera una respuesta veraz, citando la fuente y eliminando alucinaciones.

---

## 🛡️ Diseño de Arquitectura: Seguridad vía Tampermonkey

Por motivos de seguridad y para garantizar el cumplimiento de protocolos de protección de datos, la comunicación con el agente se ha implementado mediante un **Userscript (Tampermonkey)** como puente de arquitectura:

*   **Aislamiento de Capas:** El agente de IA no se expone directamente en el código fuente de la web app Shiny, protegiendo las credenciales y tokens de la API de Salesforce del acceso público.
*   **Túnel Seguro (Zero Trust):** La interacción con el **Atlas Reasoning Engine** solo se activa en estaciones de trabajo autorizadas que posean el script de control inyectado, añadiendo una capa de seguridad basada en dispositivo.
*   **Gestión de Sesión:** Tampermonkey actúa como gestor de tokens en el lado del cliente, asegurando que la conexión con el agente solo sea posible bajo una sesión autenticada y cifrada.

---

## 📊 Comparativa: IA Tradicional vs. OdontoGest RAG

| Característica | LLM Tradicional (Genérico) | OdontoGest RAG (Salesforce) |
| :--- | :--- | :--- |
| **Fuente de Información** | Datos de entrenamiento estáticos | Documentación clínica dinámica |
| **Veracidad** | Propenso a alucinaciones | Respuestas basadas en evidencia |
| **Privacidad** | Riesgo de fuga de datos | Salesforce Trust Layer (Privado) |
| **Implementación** | Inserción directa en código | Inyección segura vía Tampermonkey |

---

## 🛠️ Configuración Técnica en el Repositorio

Para habilitar la comunicación entre el dashboard en **R Shiny** y el agente de **Salesforce** (gestionado por el script de Tampermonkey), el archivo de configuración debe contener:

```env
# Salesforce & RAG Configuration
SALESFORCE_ORG_ID="00DWs00000MLtHl"
SALESFORCE_SITE_URL="[https://storm-9d7318d2748761.my.site.com/](https://storm-9d7318d2748761.my.site.com/)..."
SALESFORCE_SCRT_URL="[https://storm-9d7318d2748761.my.salesforce-scrt.com](https://storm-9d7318d2748761.my.salesforce-scrt.com)"
SALESFORCE_TOKEN_ENCRYPTION="sodium_encrypted_key"

# Client-Side Security (Tampermonkey)
VERSION_SCRIPT="2.0"
AUTHOR="JGG"
```
