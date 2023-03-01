/**El código es un programa de simulación de hidrología que utiliza el método de Muskingum para calcular el flujo de agua en un canal y los almacenamientos en un embalse y en el suelo. Los parámetros de entrada incluyen la precipitación, la evapotranspiración y el caudal observado, mientras que los parámetros de salida incluyen los almacenamientos en el embalse, en el canal y en el suelo.

El programa también calcula tres índices de desempeño: PBIAS, NSE y R2. PBIAS mide el sesgo relativo de la estimación del caudal, NSE mide la capacidad del modelo para reproducir la variabilidad del caudal observado y R2 mide la correlación entre el caudal observado y el caudal simulado.

El programa define varias funciones para realizar los cálculos necesarios. La función calcularFlujoCanal calcula el flujo de agua a través del canal utilizando el método de Muskingum. La función calcularAlmacenamientoEmbalse calcula el almacenamiento en el embalse. La función calcularAlmacenamientoCanal calcula el almacenamiento en el canal. La función calcularAlmacenamientoSuelo calcula el almacenamiento en el suelo. La función calcularIndicesDesempeno calcula los tres índices de desempeño.

En la función principal del programa, el usuario proporciona los parámetros del modelo, incluyendo el coeficiente de drenaje superficial, el coeficiente de infiltración, el coeficiente de almacenamiento en el embalse, el coeficiente de almacenamiento en el canal, el coeficiente de almacenamiento en el suelo y la constante de tiempo del método de Muskingum. A continuación, el programa solicita la precipitación, la evapotranspiración y el caudal observado para cada período de tiempo. Con estos datos, el programa realiza los cálculos necesarios para obtener los almacenamientos en el embalse, en el canal y en el suelo, así como el flujo de agua a través del canal. Finalmente, el programa calcula los índices de desempeño y los muestra al usuario.

El codigo puede correr en cualquier compilador de C++. El codigo fue probado en https://www.onlinegdb.com/online_c++_compiler
*****************************************/

#include <iostream>
#include <cmath>
#include <vector>

using namespace std;

// Definir estructura de datos para los parámetros del modelo
struct ParametrosModelo {
    double Ks;  // Coeficiente de drenaje superficial
    double Kc;  // Coeficiente de infiltración
    double Kb;  // Coeficiente de almacenamiento en el embalse
    double Kw;  // Coeficiente de almacenamiento en el canal
    double Kz;  // Coeficiente de almacenamiento en el suelo
    double C;   // Constante de tiempo del método de Muskingum
};

// Definir los datos de entrada (precipitación, evapotranspiración y caudal observado)
struct DatosEntrada {
    double precipitacion;
    double evapotranspiracion;
    double caudalObservado;
};

// Definir los datos de salida (almacenamiento en el embalse, almacenamiento en el canal y almacenamiento en el suelo)
struct DatosSalida {
    double almacenamientoEmbalse;
    double almacenamientoCanal;
    double almacenamientoSuelo;
};

// Definir los parámetros de los índices de desempeño
struct IndicesDesempeno {
    double PBIAS;
    double NSE;
    double R2;
};

// Definir la función para calcular el flujo de agua a través del canal usando el método de Muskingum
double calcularFlujoCanal(double almacenamientoCanalAnterior, double almacenamientoSueloAnterior, double caudalObservado, double Kc, double Kw, double C) {
    double flujoCanal;
    flujoCanal = (Kc * almacenamientoSueloAnterior + Kw * almacenamientoCanalAnterior + C * caudalObservado) / (Kc + Kw + C);
    return flujoCanal;
}

// Definir la función para calcular el almacenamiento en el embalse
double calcularAlmacenamientoEmbalse(double almacenamientoEmbalseAnterior, double precipitacion, double evapotranspiracion, double Kb) {
    double almacenamientoEmbalse;
    almacenamientoEmbalse = almacenamientoEmbalseAnterior + precipitacion - evapotranspiracion - Kb;
    return almacenamientoEmbalse;
}

// Definir la función para calcular el almacenamiento en el canal
double calcularAlmacenamientoCanal(double almacenamientoCanalAnterior, double flujoCanal, double Kc) {
    double almacenamientoCanal;
    almacenamientoCanal = almacenamientoCanalAnterior + Kc * flujoCanal;
    return almacenamientoCanal;
}

// Definir la función para calcular el almacenamiento en el suelo
double calcularAlmacenamientoSuelo(double almacenamientoSueloAnterior, double precipitacion, double evapotranspiracion, double Ks, double Kz) {
    double almacenamientoSuelo;
    almacenamientoSuelo = almacenamientoSueloAnterior + Ks * precipitacion - Kz * evapotranspiracion;
    return almacenamientoSuelo;
}

// Definir la función para calcular los índices de desempeño
IndicesDesempeno calcularIndicesDesempeno(vector<double> caudalObservado, vector<double> caudalCalculado) {
    IndicesDesempeno indicesDesempeno;
    double sumObservado = 0;
    double sumCalculado = 0;
    double sumObservadoCuadrado = 0;
    double sumCalculadoCuadrado = 0;
    double sumObservadoCalculado = 0;
    int n = caudalObservado.size();

    // Calcular PBIAS
    for (int i=0;i<n;i++) {
        sumObservado += caudalObservado[i];
        sumCalculado += caudalCalculado[i];
    }
    indicesDesempeno.PBIAS = (sumCalculado - sumObservado) / sumObservado;

    // Calcular NSE
    for (int i=0;i<n;i++) {
        sumObservadoCuadrado += caudalObservado[i] * caudalObservado[i];
        sumCalculadoCuadrado += caudalCalculado[i] * caudalCalculado[i];
        sumObservadoCalculado += caudalObservado[i] * caudalCalculado[i];
    }
    indicesDesempeno.NSE = 1 - (sumCalculadoCuadrado + sumObservadoCuadrado - (2 * sumObservadoCalculado)) / (sumObservadoCuadrado - (sumObservado * sumObservado) / n);

    // Calcular R2
    indicesDesempeno.R2 = 1 - (sumCalculadoCuadrado - (2 * sumObservadoCalculado) + sumObservadoCuadrado) / sumObservadoCuadrado;

    return indicesDesempeno;
}

// Definir la función principal del programa
int main() {
    // Solicitar los parámetros del modelo al usuario
    ParametrosModelo parametrosModelo;
    cout << "Ingrese el coeficiente de drenaje superficial (Ks): ";
    cin >> parametrosModelo.Ks;
    cout << "Ingrese el coeficiente de infiltración (Kc): ";
    cin >> parametrosModelo.Kc;
    cout << "Ingrese el coeficiente de almacenamiento en el embalse (Kb): ";
    cin >> parametrosModelo.Kb;
    cout << "Ingrese el coeficiente de almacenamiento en el canal (Kw): ";
    cin >> parametrosModelo.Kw;
    cout << "Ingrese el coeficiente de almacenamiento en el suelo (Kz): ";
    cin >> parametrosModelo.Kz;
    cout << "Ingrese la constante de tiempo del método de Muskingum (C): ";
    cin >> parametrosModelo.C;

    // Solicitar los datos de entrada al usuario
    vector<DatosEntrada> datosEntrada;
    DatosEntrada datoEntrada;
    int numDatos;
    cout << "Ingrese el número de datos: ";
    cin >> numDatos;
    cout << "Ingrese los datos de entrada: " << endl;
    for (int i=0;i<numDatos;i++) {
        cout << "Precipitación " << i+1 << ": ";
        cin >> datoEntrada.precipitacion;
        cout << "Evapotranspiración " << i+1 << ": ";
        cin >> datoEntrada.evapotranspiracion;
        cout << "Caudal observado " << i+1 << ": ";
        cin >> datoEntrada.caudalObservado;
        datosEntrada.push_back(datoEntrada);
    }

    // Definir los vectores para almacenar los datos de salida y los índices de desempeño
    vector<DatosSalida> datosSalida;
    vector<IndicesDesempeno> indicesDesempeno;
    vector<double> caudalObservado;
    vector<double> caudalCalculado;

    // Inicializar variables
    DatosSalida datoSalida;
    double almacenamientoEmbalseAnterior = 0;
    double almacenamientoCanalAnterior = 0;
    double almacenamientoSueloAnterior = 0;

    // Iterar sobre los datos de entrada
    for (int i=0;i<numDatos;i++) {
        // Calcular los datos de salida usando las subrutinas
        datoSalida.almacenamientoEmbalse = calcularAlmacenamientoEmbalse(almacenamientoEmbalseAnterior, datosEntrada[i].precipitacion, datosEntrada[i].evapotranspiracion, parametrosModelo.Kb);
        double flujoCanal = calcularFlujoCanal(almacenamientoCanalAnterior, almacenamientoSueloAnterior, datosEntrada[i].caudalObservado, parametrosModelo.Kc, parametrosModelo.Kw, parametrosModelo.C);
        datoSalida.almacenamientoCanal = calcularAlmacenamientoCanal(almacenamientoCanalAnterior, flujoCanal, parametrosModelo.Kc);
        datoSalida.almacenamientoSuelo = calcularAlmacenamientoSuelo(almacenamientoSueloAnterior, datosEntrada[i].precipitacion, datosEntrada[i].evapotranspiracion, parametrosModelo.Ks, parametrosModelo.Kz);
        datosSalida.push_back(datoSalida);

        // Actualizar las variables
        almacenamientoEmbalseAnterior = datoSalida.almacenamientoEmbalse;
        almacenamientoCanalAnterior = datoSalida.almacenamientoCanal;
        almacenamientoSueloAnterior = datoSalida.almacenamientoSuelo;

        // Almacenar los datos de caudal
        caudalObservado.push_back(datosEntrada[i].caudalObservado);
        caudalCalculado.push_back(datoSalida.almacenamientoCanal + datoSalida.almacenamientoSuelo);
    }

    // Calcular los índices de desempeño
    IndicesDesempeno indiceDesempeno;
    indiceDesempeno = calcularIndicesDesempeno(caudalObservado, caudalCalculado);
    indicesDesempeno.push_back(indiceDesempeno);

    // Imprimir los resultados
    cout << endl << "Los resultados son:" << endl;
    cout << "Almacenamiento en el embalse: ";
    for (int i=0;i<numDatos;i++) {
        cout << datosSalida[i].almacenamientoEmbalse << " ";
    }
    cout << endl;

    cout << "Almacenamiento en el canal: ";
    for (int i=0;i<numDatos;i++) {
        cout << datosSalida[i].almacenamientoCanal << " ";
    }
    cout << endl;

    cout << "Almacenamiento en el suelo: ";
    for (int i=0;i<numDatos;i++) {
        cout << datosSalida[i].almacenamientoSuelo << " ";
    }
    cout << endl << endl;

    cout << "Índice PBIAS: " << indicesDesempeno[0].PBIAS << endl;
    cout << "Índice NSE: " << indicesDesempeno[0].NSE << endl;
    cout << "Índice R2: " << indicesDesempeno[0].R2 << endl;

    return 0;
}
