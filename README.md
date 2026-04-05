# OdontoGest: Intelligent Clinical Management System with CDSS and RAG-based AI

**Final Degree Project (TFG)**  
**A Data-Driven Approach to Clinical Decision Support using Machine Learning, Explainable AI, and Retrieval-Augmented Generation**

---

## 📌 Abstract

This project presents the design, development, and evaluation of an intelligent clinical platform that integrates a **Dental Management System (OdontoGest)** with a **Clinical Decision Support System (CDSS)** enhanced by **Machine Learning (ML), Explainable Artificial Intelligence (XAI), and Retrieval-Augmented Generation (RAG)**.

The system addresses inefficiencies in traditional clinical workflows by introducing a **proactive, data-driven paradigm**. It combines structured clinical data with unstructured knowledge sources to support both operational management and clinical decision-making.

The results demonstrate that predictive modeling improves efficiency in inventory management and diagnostic classification, while RAG-based AI enhances the reliability and contextual accuracy of generated responses.

---

## 🎯 Research Objectives

- Design and implement a modular clinical management system  
- Develop predictive models for:
  - Inventory demand forecasting  
  - Clinical diagnosis classification  
- Ensure interpretability through Explainable AI (XAI)  
- Implement a RAG-based generative AI assistant  
- Evaluate system performance using quantitative and qualitative metrics  

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

---

## 🧠 Artificial Intelligence Architecture

### 🔹 Predictive Modeling

#### 📦 Inventory Forecasting
- Model: **Log-Linear Regression**
- Inputs:
  - Historical consumption  
  - Patient volume  

**Metrics:**
- RMSE  
- MAE  

---

#### 🩺 Clinical Diagnosis Classification
- Model: **XGBoost**
- Inputs:
  - Plaque index  
  - Bleeding  
  - Pocket depth  

**Metrics:**
- Accuracy  
- Precision  
- Recall  
- F1-score  

---

### 📊 Experimental Results

| Model                     | Metric       | Value (Example) |
|--------------------------|-------------|-----------------|
| Inventory Forecasting    | RMSE        | 12.4            |
|                          | MAE         | 9.1             |
| Clinical Classification  | Accuracy    | 0.89            |
|                          | F1-score    | 0.87            |

> ⚠️ Replace these values with your real experimental results

---

### 🔍 Explainable AI (XAI)

The system integrates **SHAP (SHapley Additive Explanations)**:

- Local interpretability (per prediction)  
- Global feature importance  
- Transparency for clinical validation  

---

## 🤖 Retrieval-Augmented Generation (RAG)

### 📌 Concept

RAG combines:

- Information Retrieval (vector search)  
- Generative AI (LLMs)  

It enables grounded responses using external knowledge, reducing hallucinations.

---

### ⚙️ Implementation

An intelligent assistant named:

👉 **Dietencito**

has been developed using a RAG-based pipeline.

---

### 🔄 RAG Pipeline

1. **Data Ingestion**
   - Clinical documents (PDFs, guidelines, protocols)

2. **Preprocessing**
   - Text cleaning  
   - Chunking  

3. **Embedding Generation**
   - Vector representations  

4. **Vector Storage**
   - Semantic index  

5. **Retrieval**
   - Similarity search  

6. **Context Injection**
   - Retrieved documents added to prompt  

7. **Generation**
   - Context-aware responses  

---

### 📊 RAG Evaluation

Evaluated using:

- Relevance of retrieved documents  
- Reduction of hallucinations  
- Contextual accuracy  

Optional metrics:
- Recall@K  
- MRR  

---

## 🖼️ System Diagrams

### 📊 General Architecture
![Architecture](docs/arquitectura.png)

---

### 🔄 RAG Pipeline
![RAG Pipeline](docs/rag_pipeline.png)

---

### 🧠 Machine Learning Workflow
![ML Workflow](docs/ml_pipeline.png)

---

## ⚙️ Technology Stack

### Backend & Data
- R 4.x  
- caret  
- xgboost  
- dplyr  
- fastshap  

### Frontend
- R Shiny  
- plotly  

### Database
- MySQL / PostgreSQL  
- pool  

### Security
- bcrypt  
- sodium  

### DevOps
- Docker  
- GitHub Actions  
- Heroku / ShinyApps.io  

---

## 📁 Repository Structure

```text
.
├── ClinicAppTFG/        # Shiny application
├── modelos/             # ML models
├── data/                # Data and schemas
├── docs/                # Diagrams
├── tests/               # Unit tests
├── .github/workflows/   # CI/CD
└── README.md
```

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

## 📚 References

- Chen, T., & Guestrin, C. (2016). *XGBoost: A Scalable Tree Boosting System*. KDD.  
- Lundberg, S. M., & Lee, S. I. (2017). *A Unified Approach to Interpreting Model Predictions*. NeurIPS.  
- Lewis, P. et al. (2020). *Retrieval-Augmented Generation for Knowledge-Intensive NLP Tasks*. NeurIPS.  
- Kuhn, M. (2008). *Building Predictive Models in R Using the caret Package*. Journal of Statistical Software.  
