#include <iostream>
#include <iomanip>
#include <cmath>
#include <string>
#include <numeric>
#include <sstream>
#include <vector>
#include <complex>
#include <algorithm>
#include <limits>
#include <cstdlib>
#include <ctime>

using namespace std;

// Ricardian Calculator
// Terminal-based scientific calculator written in C++17.
// This project is a standalone, single-file console application.
// ============================================================

// ============================================================
// RICARDIAN CALCULATOR
// C++17 - Single File
// ============================================================

const double PI = acos(-1.0);
const double E  = exp(1.0);

double ans = 0.0;

enum AngleMode {
    DEG,
    RAD,
    GRA
};

AngleMode angleMode = DEG;

// ------------------------------------------------------------
// Utility
// ------------------------------------------------------------

void clearInput() {
    cin.clear();
    cin.ignore(numeric_limits<streamsize>::max(), '\n');
}

double toRadians(double x) {
    if (angleMode == DEG)
        return x * PI / 180.0;

    if (angleMode == GRA)
        return x * PI / 200.0;

    return x;
}

double fromRadians(double x) {
    if (angleMode == DEG)
        return x * 180.0 / PI;

    if (angleMode == GRA)
        return x * 200.0 / PI;

    return x;
}

string angleName() {
    if (angleMode == DEG) return "DEG";
    if (angleMode == RAD) return "RAD";
    return "GRA";
}

void pauseScreen() {
    cout << "\nPress ENTER to continue...";
    cin.ignore(numeric_limits<streamsize>::max(), '\n');
}

// ------------------------------------------------------------
// Factorial
// ------------------------------------------------------------

double factorial(double x) {
    if (x < 0 || floor(x) != x)
        throw runtime_error("Factorial requires a non-negative integer.");

    if (x > 170)
        throw runtime_error("Number too large for factorial.");

    double result = 1.0;

    for (int i = 2; i <= static_cast<int>(x); ++i)
        result *= i;

    return result;
}

// ------------------------------------------------------------
// Expression Parser
// ------------------------------------------------------------

class Parser {
private:
    string s;
    size_t pos;

public:
    Parser(const string& input) : s(input), pos(0) {}

    void skipSpaces() {
        while (pos < s.size() && isspace(static_cast<unsigned char>(s[pos])))
            pos++;
    }

    bool match(char c) {
        skipSpaces();

        if (pos < s.size() && s[pos] == c) {
            pos++;
            return true;
        }

        return false;
    }

    double parse() {
        double value = expression();

        skipSpaces();

        if (pos != s.size())
            throw runtime_error("Unexpected character.");

        return value;
    }

private:

    double expression() {
        double value = term();

        while (true) {
            if (match('+'))
                value += term();
            else if (match('-'))
                value -= term();
            else
                break;
        }

        return value;
    }

    double term() {
        double value = power();

        while (true) {
            if (match('*'))
                value *= power();

            else if (match('/')) {
                double divisor = power();

                if (fabs(divisor) < 1e-15)
                    throw runtime_error("Division by zero.");

                value /= divisor;
            }

            else if (match('%')) {
                double divisor = power();

                if (fabs(divisor) < 1e-15)
                    throw runtime_error("Modulo by zero.");

                value = fmod(value, divisor);
            }

            else
                break;
        }

        return value;
    }

    double power() {
        double value = unary();

        if (match('^')) {
            double exponent = power();
            value = pow(value, exponent);
        }

        return value;
    }

    double unary() {
        skipSpaces();

        if (match('+'))
            return unary();

        if (match('-'))
            return -unary();

        return postfix();
    }

    double postfix() {
        double value = primary();

        while (true) {
            skipSpaces();

            if (match('!'))
                value = factorial(value);
            else
                break;
        }

        return value;
    }

    double primary() {
        skipSpaces();

        if (pos >= s.size())
            throw runtime_error("Unexpected end of expression.");

        // Parentheses
        if (match('(')) {
            double value = expression();

            if (!match(')'))
                throw runtime_error("Missing ')'.");

            return value;
        }

        // Number
        if (isdigit(static_cast<unsigned char>(s[pos])) || s[pos] == '.') {
            size_t start = pos;

            while (pos < s.size() &&
                   (isdigit(static_cast<unsigned char>(s[pos])) ||
                    s[pos] == '.' ||
                    s[pos] == 'e' ||
                    s[pos] == 'E' ||
                    s[pos] == '+' ||
                    s[pos] == '-')) {

                if ((s[pos] == '+' || s[pos] == '-') &&
                    pos > start &&
                    s[pos - 1] != 'e' &&
                    s[pos - 1] != 'E')
                    break;

                pos++;
            }

            string number = s.substr(start, pos - start);

            try {
                return stod(number);
            }
            catch (...) {
                throw runtime_error("Invalid number.");
            }
        }

        // Identifier
        if (isalpha(static_cast<unsigned char>(s[pos])) || s[pos] == '_') {
            string name;

            while (pos < s.size() &&
                   (isalnum(static_cast<unsigned char>(s[pos])) ||
                    s[pos] == '_')) {
                name += s[pos++];
            }

            for (char& c : name)
                c = static_cast<char>(tolower(static_cast<unsigned char>(c)));

            // Constants
            if (name == "pi")
                return PI;

            if (name == "e")
                return E;

            if (name == "ans")
                return ans;

            if (name == "rand")
                return static_cast<double>(rand()) / RAND_MAX;

            // Function
            if (!match('('))
                throw runtime_error("Expected '(' after function name.");

            vector<double> args;

            skipSpaces();

            if (!match(')')) {
                while (true) {
                    args.push_back(expression());

                    if (match(')'))
                        break;

                    if (!match(','))
                        throw runtime_error("Expected ','.");

                }
            }

            return callFunction(name, args);
        }

        throw runtime_error("Invalid expression.");
    }

    double callFunction(const string& name,
                        const vector<double>& a) {

        auto one = [&](const string& n) -> double {
            if (a.size() != 1)
                throw runtime_error(n + "() requires 1 argument.");

            return a[0];
        };

        if (name == "sin")
            return sin(toRadians(one(name)));

        if (name == "cos")
            return cos(toRadians(one(name)));

        if (name == "tan")
            return tan(toRadians(one(name)));

        if (name == "asin")
            return fromRadians(asin(one(name)));

        if (name == "acos")
            return fromRadians(acos(one(name)));

        if (name == "atan")
            return fromRadians(atan(one(name)));

        if (name == "sinh")
            return sinh(one(name));

        if (name == "cosh")
            return cosh(one(name));

        if (name == "tanh")
            return tanh(one(name));

        if (name == "asinh")
            return asinh(one(name));

        if (name == "acosh")
            return acosh(one(name));

        if (name == "atanh")
            return atanh(one(name));

        if (name == "sqrt")
            return sqrt(one(name));

        if (name == "cbrt")
            return cbrt(one(name));

        if (name == "abs")
            return fabs(one(name));

        if (name == "floor")
            return floor(one(name));

        if (name == "ceil")
            return ceil(one(name));

        if (name == "round")
            return round(one(name));

        if (name == "trunc")
            return trunc(one(name));

        if (name == "exp")
            return exp(one(name));

        if (name == "ln")
            return log(one(name));

        if (name == "log")
            return log10(one(name));

        if (name == "log2")
            return log2(one(name));

        if (name == "logb") {
            if (a.size() != 2)
                throw runtime_error("logb(x,base) requires 2 arguments.");

            return log(a[0]) / log(a[1]);
        }

        if (name == "recip") {
            double x = one(name);

            if (fabs(x) < 1e-15)
                throw runtime_error("Division by zero.");

            return 1.0 / x;
        }

        if (name == "mod") {
            if (a.size() != 2)
                throw runtime_error("mod(a,b) requires 2 arguments.");

            return fmod(a[0], a[1]);
        }

        if (name == "gcd") {
            if (a.size() != 2)
                throw runtime_error("gcd(a,b) requires 2 arguments.");

            long long x = static_cast<long long>(a[0]);
            long long y = static_cast<long long>(a[1]);

            return static_cast<double>(std::gcd(x, y));
        }

        if (name == "lcm") {
            if (a.size() != 2)
                throw runtime_error("lcm(a,b) requires 2 arguments.");

            long long x = static_cast<long long>(a[0]);
            long long y = static_cast<long long>(a[1]);

            return static_cast<double>(std::lcm(x, y));
        }

        if (name == "ncr") {
            if (a.size() != 2)
                throw runtime_error("ncr(n,r) requires 2 arguments.");

            int n = static_cast<int>(a[0]);
            int r = static_cast<int>(a[1]);

            if (r < 0 || n < 0 || r > n)
                throw runtime_error("Invalid nCr.");

            if (r > n - r)
                r = n - r;

            double result = 1;

            for (int i = 1; i <= r; ++i)
                result *= static_cast<double>(n - r + i) / i;

            return result;
        }

        if (name == "npr") {
            if (a.size() != 2)
                throw runtime_error("npr(n,r) requires 2 arguments.");

            int n = static_cast<int>(a[0]);
            int r = static_cast<int>(a[1]);

            if (r < 0 || n < 0 || r > n)
                throw runtime_error("Invalid nPr.");

            double result = 1;

            for (int i = 0; i < r; ++i)
                result *= (n - i);

            return result;
        }

        if (name == "min") {
            if (a.empty())
                throw runtime_error("min requires arguments.");

            return *min_element(a.begin(), a.end());
        }

        if (name == "max") {
            if (a.empty())
                throw runtime_error("max requires arguments.");

            return *max_element(a.begin(), a.end());
        }

        throw runtime_error("Unknown function: " + name);
    }
};

// ------------------------------------------------------------
// Evaluate expression
// ------------------------------------------------------------

double evaluate(const string& expression) {
    Parser parser(expression);
    return parser.parse();
}

// ------------------------------------------------------------
// Display
// ------------------------------------------------------------

void header(const string& title) {
    cout << "\n";
    cout << "============================================================\n";
    cout << "                 " << title << "\n";
    cout << "============================================================\n";
}

// ------------------------------------------------------------
// Scientific Calculator
// ------------------------------------------------------------

void scientificMode() {

    while (true) {

        header("RICARDIAN CALCULATOR");

        cout << "Angle Mode : " << angleName() << "\n";
        cout << "Ans        : " << setprecision(12) << ans << "\n\n";

        cout << "Examples:\n";
        cout << "  2+3*4\n";
        cout << "  sin(30)\n";
        cout << "  sqrt(144)\n";
        cout << "  2^10\n";
        cout << "  5!\n";
        cout << "  log(1000)\n";
        cout << "  ncr(10,3)\n\n";

        cout << "Commands:\n";
        cout << "  mode     Change DEG/RAD/GRA\n";
        cout << "  help     Show functions\n";
        cout << "  back     Return to main menu\n\n";

        cout << "calc> ";

        string input;
        getline(cin, input);

        if (input == "back")
            return;

        if (input == "mode") {

            cout << "\n1. DEG\n";
            cout << "2. RAD\n";
            cout << "3. GRA\n";
            cout << "Select: ";

            int choice;

            if (!(cin >> choice)) {
                clearInput();
                continue;
            }

            clearInput();

            if (choice == 1)
                angleMode = DEG;
            else if (choice == 2)
                angleMode = RAD;
            else if (choice == 3)
                angleMode = GRA;

            continue;
        }

        if (input == "help") {

            header("FUNCTIONS");

            cout << "Arithmetic:\n";
            cout << "  +  -  *  /  %  ^  !\n\n";

            cout << "Trigonometry:\n";
            cout << "  sin(x) cos(x) tan(x)\n";
            cout << "  asin(x) acos(x) atan(x)\n\n";

            cout << "Hyperbolic:\n";
            cout << "  sinh(x) cosh(x) tanh(x)\n";
            cout << "  asinh(x) acosh(x) atanh(x)\n\n";

            cout << "Logarithms:\n";
            cout << "  ln(x) log(x) log2(x) logb(x,b)\n\n";

            cout << "Other:\n";
            cout << "  sqrt(x) cbrt(x) abs(x)\n";
            cout << "  floor(x) ceil(x) round(x) trunc(x)\n";
            cout << "  exp(x) recip(x)\n";
            cout << "  gcd(a,b) lcm(a,b)\n";
            cout << "  ncr(n,r) npr(n,r)\n";
            cout << "  min(a,b,...) max(a,b,...)\n";
            cout << "  mod(a,b)\n\n";

            cout << "Constants:\n";
            cout << "  pi   e   ans\n";

            pauseScreen();
            continue;
        }

        if (input.empty())
            continue;

        try {
            ans = evaluate(input);

            cout << "\nResult = "
                 << setprecision(15)
                 << ans << "\n";

        }
        catch (const exception& ex) {
            cout << "\nError: " << ex.what() << "\n";
        }
    }
}

// ------------------------------------------------------------
// Complex Calculator
// ------------------------------------------------------------

void complexMode() {

    while (true) {

        header("COMPLEX NUMBER CALCULATOR");

        cout << "Enter complex numbers as:\n";
        cout << "  a+bi\n";
        cout << "  a-bi\n\n";

        cout << "1. Add\n";
        cout << "2. Subtract\n";
        cout << "3. Multiply\n";
        cout << "4. Divide\n";
        cout << "5. Magnitude\n";
        cout << "6. Conjugate\n";
        cout << "7. Back\n\n";

        cout << "Select: ";

        int choice;

        if (!(cin >> choice)) {
            clearInput();
            continue;
        }

        clearInput();

        if (choice == 7)
            return;

        if (choice >= 1 && choice <= 4) {

            double a, b, c, d;

            cout << "First real part: ";
            cin >> a;

            cout << "First imaginary part: ";
            cin >> b;

            cout << "Second real part: ";
            cin >> c;

            cout << "Second imaginary part: ";
            cin >> d;

            clearInput();

            complex<double> z1(a, b);
            complex<double> z2(c, d);
            complex<double> result;

            if (choice == 1)
                result = z1 + z2;

            else if (choice == 2)
                result = z1 - z2;

            else if (choice == 3)
                result = z1 * z2;

            else {
                if (abs(z2) < 1e-15) {
                    cout << "Division by zero.\n";
                    pauseScreen();
                    continue;
                }

                result = z1 / z2;
            }

            cout << "\nResult = "
                 << result.real();

            if (result.imag() >= 0)
                cout << "+";

            cout << result.imag() << "i\n";

            pauseScreen();
        }

        else if (choice == 5) {

            double a, b;

            cout << "Real part: ";
            cin >> a;

            cout << "Imaginary part: ";
            cin >> b;

            clearInput();

            cout << "Magnitude = "
                 << abs(complex<double>(a, b))
                 << "\n";

            pauseScreen();
        }

        else if (choice == 6) {

            double a, b;

            cout << "Real part: ";
            cin >> a;

            cout << "Imaginary part: ";
            cin >> b;

            clearInput();

            complex<double> z(a, b);
            complex<double> result = conj(z);

            cout << "Conjugate = "
                 << result.real();

            if (result.imag() >= 0)
                cout << "+";

            cout << result.imag() << "i\n";

            pauseScreen();
        }
    }
}

// ------------------------------------------------------------
// Statistics
// ------------------------------------------------------------

void statisticsMode() {

    while (true) {

        header("STATISTICS");

        cout << "1. One-variable statistics\n";
        cout << "2. Linear regression\n";
        cout << "3. Quadratic regression\n";
        cout << "4. Back\n\n";

        cout << "Select: ";

        int choice;

        if (!(cin >> choice)) {
            clearInput();
            continue;
        }

        clearInput();

        if (choice == 4)
            return;

        if (choice == 1) {

            int n;

            cout << "Number of values: ";
            cin >> n;

            if (n <= 0) {
                clearInput();
                continue;
            }

            vector<double> x(n);

            for (int i = 0; i < n; ++i) {
                cout << "x[" << i + 1 << "] = ";
                cin >> x[i];
            }

            clearInput();

            double sum = 0;

            for (double v : x)
                sum += v;

            double mean = sum / n;

            double variance = 0;

            for (double v : x)
                variance += (v - mean) * (v - mean);

            variance /= n;

            double sampleVariance = 0;

            if (n > 1)
                sampleVariance =
                    accumulate(x.begin(), x.end(), 0.0,
                        [mean](double total, double value) {
                            return total + (value - mean) * (value - mean);
                        }) / (n - 1);

            auto minmax =
                minmax_element(x.begin(), x.end());

            cout << "\nCount = " << n << "\n";
            cout << "Sum   = " << sum << "\n";
            cout << "Mean  = " << mean << "\n";
            cout << "Min   = " << *minmax.first << "\n";
            cout << "Max   = " << *minmax.second << "\n";
            cout << "Range = " << *minmax.second - *minmax.first << "\n";
            cout << "Population variance = " << variance << "\n";
            cout << "Population SD       = " << sqrt(variance) << "\n";

            if (n > 1)
                cout << "Sample SD           = "
                     << sqrt(sampleVariance) << "\n";

            pauseScreen();
        }

        else if (choice == 2) {

            int n;

            cout << "Number of points: ";
            cin >> n;

            if (n < 2) {
                clearInput();
                continue;
            }

            vector<double> x(n), y(n);

            for (int i = 0; i < n; ++i) {
                cout << "x[" << i + 1 << "] = ";
                cin >> x[i];

                cout << "y[" << i + 1 << "] = ";
                cin >> y[i];
            }

            clearInput();

            double sx = 0, sy = 0;
            double sxx = 0, sxy = 0;

            for (int i = 0; i < n; ++i) {
                sx += x[i];
                sy += y[i];
                sxx += x[i] * x[i];
                sxy += x[i] * y[i];
            }

            double denominator =
                n * sxx - sx * sx;

            if (fabs(denominator) < 1e-15) {
                cout << "Cannot calculate regression.\n";
                pauseScreen();
                continue;
            }

            double slope =
                (n * sxy - sx * sy) / denominator;

            double intercept =
                (sy - slope * sx) / n;

            cout << "\nRegression equation:\n";
            cout << "y = " << slope
                 << "x + " << intercept << "\n";

            pauseScreen();
        }

        else if (choice == 3) {

            cout << "\nQuadratic regression uses the form:\n";
            cout << "y = ax^2 + bx + c\n\n";

            int n;

            cout << "Number of points: ";
            cin >> n;

            if (n < 3) {
                clearInput();
                continue;
            }

            vector<double> x(n), y(n);

            for (int i = 0; i < n; ++i) {
                cout << "x[" << i + 1 << "] = ";
                cin >> x[i];

                cout << "y[" << i + 1 << "] = ";
                cin >> y[i];
            }

            clearInput();

            double Sx = 0;
            double Sx2 = 0;
            double Sx3 = 0;
            double Sx4 = 0;

            double Sy = 0;
            double Sxy = 0;
            double Sx2y = 0;

            for (int i = 0; i < n; ++i) {

                double x2 = x[i] * x[i];

                Sx += x[i];
                Sx2 += x2;
                Sx3 += x2 * x[i];
                Sx4 += x2 * x2;

                Sy += y[i];
                Sxy += x[i] * y[i];
                Sx2y += x2 * y[i];
            }

            // Gaussian elimination
            double A[3][4] = {
                {Sx4, Sx3, Sx2, Sx2y},
                {Sx3, Sx2, Sx,  Sxy},
                {Sx2, Sx,  static_cast<double>(n), Sy}
            };

            for (int i = 0; i < 3; ++i) {

                int pivot = i;

                for (int j = i + 1; j < 3; ++j) {
                    if (fabs(A[j][i]) > fabs(A[pivot][i]))
                        pivot = j;
                }

                if (fabs(A[pivot][i]) < 1e-15) {
                    cout << "Cannot calculate regression.\n";
                    pauseScreen();
                    goto quadratic_done;
                }

                for (int j = i; j < 4; ++j)
                    swap(A[i][j], A[pivot][j]);

                double div = A[i][i];

                for (int j = i; j < 4; ++j)
                    A[i][j] /= div;

                for (int k = 0; k < 3; ++k) {

                    if (k == i)
                        continue;

                    double factor = A[k][i];

                    for (int j = i; j < 4; ++j)
                        A[k][j] -= factor * A[i][j];
                }
            }

            cout << "\na = " << A[0][3] << "\n";
            cout << "b = " << A[1][3] << "\n";
            cout << "c = " << A[2][3] << "\n";

            cout << "\ny = "
                 << A[0][3] << "x^2 + "
                 << A[1][3] << "x + "
                 << A[2][3] << "\n";

            quadratic_done:

            pauseScreen();
        }
    }
}

// ------------------------------------------------------------
// Matrix
// ------------------------------------------------------------

using Matrix = vector<vector<double>>;

Matrix readMatrix() {

    int rows, cols;

    cout << "Rows: ";
    cin >> rows;

    cout << "Columns: ";
    cin >> cols;

    Matrix A(rows, vector<double>(cols));

    for (int i = 0; i < rows; ++i) {

        for (int j = 0; j < cols; ++j) {

            cout << "A[" << i + 1
                 << "][" << j + 1 << "] = ";

            cin >> A[i][j];
        }
    }

    return A;
}

void printMatrix(const Matrix& A) {

    cout << "\n";

    for (const auto& row : A) {

        for (double value : row)
            cout << setw(12)
                 << setprecision(6)
                 << value;

        cout << "\n";
    }
}

void matrixMode() {

    while (true) {

        header("MATRIX CALCULATOR");

        cout << "1. Add\n";
        cout << "2. Subtract\n";
        cout << "3. Multiply\n";
        cout << "4. Transpose\n";
        cout << "5. Determinant\n";
        cout << "6. Identity matrix\n";
        cout << "7. Back\n\n";

        cout << "Select: ";

        int choice;

        if (!(cin >> choice)) {
            clearInput();
            continue;
        }

        if (choice == 7) {
            clearInput();
            return;
        }

        if (choice == 1 || choice == 2) {

            cout << "\nMatrix A\n";
            Matrix A = readMatrix();

            cout << "\nMatrix B\n";
            Matrix B = readMatrix();

            clearInput();

            if (A.size() != B.size() ||
                A[0].size() != B[0].size()) {

                cout << "Matrix dimensions do not match.\n";
                pauseScreen();
                continue;
            }

            Matrix C = A;

            for (size_t i = 0; i < A.size(); ++i)
                for (size_t j = 0; j < A[0].size(); ++j)
                    C[i][j] =
                        choice == 1
                        ? A[i][j] + B[i][j]
                        : A[i][j] - B[i][j];

            cout << "\nResult:";
            printMatrix(C);

            pauseScreen();
        }

        else if (choice == 3) {

            cout << "\nMatrix A\n";
            Matrix A = readMatrix();

            cout << "\nMatrix B\n";
            Matrix B = readMatrix();

            clearInput();

            if (A[0].size() != B.size()) {

                cout << "Invalid matrix dimensions.\n";
                pauseScreen();
                continue;
            }

            Matrix C(
                A.size(),
                vector<double>(B[0].size(), 0)
            );

            for (size_t i = 0; i < A.size(); ++i)
                for (size_t j = 0; j < B[0].size(); ++j)
                    for (size_t k = 0; k < B.size(); ++k)
                        C[i][j] +=
                            A[i][k] * B[k][j];

            cout << "\nResult:";
            printMatrix(C);

            pauseScreen();
        }

        else if (choice == 4) {

            Matrix A = readMatrix();

            clearInput();

            Matrix T(
                A[0].size(),
                vector<double>(A.size())
            );

            for (size_t i = 0; i < A.size(); ++i)
                for (size_t j = 0; j < A[0].size(); ++j)
                    T[j][i] = A[i][j];

            cout << "\nTranspose:";
            printMatrix(T);

            pauseScreen();
        }

        else if (choice == 5) {

            Matrix A = readMatrix();

            clearInput();

            if (A.size() != A[0].size()) {

                cout << "Matrix must be square.\n";
                pauseScreen();
                continue;
            }

            int n = static_cast<int>(A.size());

            double det = 1.0;

            for (int i = 0; i < n; ++i) {

                int pivot = i;

                for (int j = i + 1; j < n; ++j)
                    if (fabs(A[j][i]) > fabs(A[pivot][i]))
                        pivot = j;

                if (fabs(A[pivot][i]) < 1e-15) {
                    det = 0;
                    break;
                }

                if (pivot != i) {
                    swap(A[pivot], A[i]);
                    det *= -1;
                }

                det *= A[i][i];

                for (int j = i + 1; j < n; ++j) {

                    double factor =
                        A[j][i] / A[i][i];

                    for (int k = i; k < n; ++k)
                        A[j][k] -= factor * A[i][k];
                }
            }

            cout << "\nDeterminant = "
                 << det << "\n";

            pauseScreen();
        }

        else if (choice == 6) {

            int n;

            cout << "Size of identity matrix: ";
            cin >> n;

            clearInput();

            Matrix I(
                n,
                vector<double>(n, 0)
            );

            for (int i = 0; i < n; ++i)
                I[i][i] = 1;

            printMatrix(I);

            pauseScreen();
        }
    }
}

// ------------------------------------------------------------
// Vector
// ------------------------------------------------------------

void vectorMode() {

    while (true) {

        header("VECTOR CALCULATOR");

        cout << "1. Dot product\n";
        cout << "2. Cross product\n";
        cout << "3. Magnitude\n";
        cout << "4. Angle between vectors\n";
        cout << "5. Back\n\n";

        cout << "Select: ";

        int choice;

        if (!(cin >> choice)) {
            clearInput();
            continue;
        }

        clearInput();

        if (choice == 5)
            return;

        int n;

        cout << "Dimension (2 or 3): ";
        cin >> n;

        if (n != 2 && n != 3) {
            clearInput();
            cout << "Only 2D and 3D vectors are supported.\n";
            pauseScreen();
            continue;
        }

        vector<double> A(n);

        cout << "\nVector A:\n";

        for (int i = 0; i < n; ++i)
            cin >> A[i];

        if (choice == 3) {

            clearInput();

            double sum = 0;

            for (double x : A)
                sum += x * x;

            cout << "Magnitude = "
                 << sqrt(sum) << "\n";

            pauseScreen();
            continue;
        }

        vector<double> B(n);

        cout << "\nVector B:\n";

        for (int i = 0; i < n; ++i)
            cin >> B[i];

        clearInput();

        if (choice == 1) {

            double dot = 0;

            for (int i = 0; i < n; ++i)
                dot += A[i] * B[i];

            cout << "Dot product = "
                 << dot << "\n";
        }

        else if (choice == 2) {

            if (n != 3) {

                cout << "Cross product requires 3D vectors.\n";

            }
            else {

                double x =
                    A[1] * B[2] -
                    A[2] * B[1];

                double y =
                    A[2] * B[0] -
                    A[0] * B[2];

                double z =
                    A[0] * B[1] -
                    A[1] * B[0];

                cout << "Cross product = ("
                     << x << ", "
                     << y << ", "
                     << z << ")\n";
            }
        }

        else if (choice == 4) {

            double dot = 0;
            double magA = 0;
            double magB = 0;

            for (int i = 0; i < n; ++i) {

                dot += A[i] * B[i];
                magA += A[i] * A[i];
                magB += B[i] * B[i];
            }

            magA = sqrt(magA);
            magB = sqrt(magB);

            if (magA < 1e-15 || magB < 1e-15) {

                cout << "Zero vector has no direction.\n";

            }
            else {

                double c =
                    dot / (magA * magB);

                c = max(-1.0, min(1.0, c));

                cout << "Angle = "
                     << fromRadians(acos(c))
                     << " " << angleName()
                     << "\n";
            }
        }

        pauseScreen();
    }
}

// ------------------------------------------------------------
// Equation Solver
// ------------------------------------------------------------

void equationMode() {

    while (true) {

        header("EQUATION SOLVER");

        cout << "1. Quadratic equation\n";
        cout << "2. Two simultaneous linear equations\n";
        cout << "3. Back\n\n";

        cout << "Select: ";

        int choice;

        if (!(cin >> choice)) {
            clearInput();
            continue;
        }

        clearInput();

        if (choice == 3)
            return;

        if (choice == 1) {

            double a, b, c;

            cout << "\nEquation: ax^2 + bx + c = 0\n\n";

            cout << "a = ";
            cin >> a;

            cout << "b = ";
            cin >> b;

            cout << "c = ";
            cin >> c;

            clearInput();

            if (fabs(a) < 1e-15) {

                if (fabs(b) < 1e-15) {

                    cout << "Not a valid equation.\n";

                }
                else {

                    cout << "x = "
                         << -c / b << "\n";
                }

                pauseScreen();
                continue;
            }

            double discriminant =
                b * b - 4 * a * c;

            if (discriminant > 0) {

                double x1 =
                    (-b + sqrt(discriminant)) /
                    (2 * a);

                double x2 =
                    (-b - sqrt(discriminant)) /
                    (2 * a);

                cout << "x1 = " << x1 << "\n";
                cout << "x2 = " << x2 << "\n";
            }

            else if (fabs(discriminant) < 1e-15) {

                double x = -b / (2 * a);

                cout << "x = " << x << "\n";
            }

            else {

                double real =
                    -b / (2 * a);

                double imaginary =
                    sqrt(-discriminant) /
                    (2 * a);

                cout << "x1 = "
                     << real << " + "
                     << imaginary << "i\n";

                cout << "x2 = "
                     << real << " - "
                     << imaginary << "i\n";
            }

            pauseScreen();
        }

        else if (choice == 2) {

            cout << "\nEquations:\n";
            cout << "a1*x + b1*y = c1\n";
            cout << "a2*x + b2*y = c2\n\n";

            double a1, b1, c1;
            double a2, b2, c2;

            cout << "a1 = ";
            cin >> a1;

            cout << "b1 = ";
            cin >> b1;

            cout << "c1 = ";
            cin >> c1;

            cout << "a2 = ";
            cin >> a2;

            cout << "b2 = ";
            cin >> b2;

            cout << "c2 = ";
            cin >> c2;

            clearInput();

            double determinant =
                a1 * b2 - a2 * b1;

            if (fabs(determinant) < 1e-15) {

                cout << "No unique solution.\n";

            }
            else {

                double x =
                    (c1 * b2 - c2 * b1) /
                    determinant;

                double y =
                    (a1 * c2 - a2 * c1) /
                    determinant;

                cout << "\nx = " << x << "\n";
                cout << "y = " << y << "\n";
            }

            pauseScreen();
        }
    }
}

// ------------------------------------------------------------
// Numerical Calculus
// ------------------------------------------------------------

void calculusMode() {

    while (true) {

        header("NUMERICAL CALCULUS");

        cout << "1. Numerical derivative\n";
        cout << "2. Numerical integration\n";
        cout << "3. Back\n\n";

        cout << "Select: ";

        int choice;

        if (!(cin >> choice)) {
            clearInput();
            continue;
        }

        clearInput();

        if (choice == 3)
            return;

        if (choice == 1) {

            string expression;
            double x, h;

            cout << "\nEnter f(x).\n";
            cout << "Use 'x' as the variable.\n";
            cout << "Example: x^2+sin(x)\n\n";

            cout << "f(x) = ";
            getline(cin, expression);

            cout << "x = ";
            cin >> x;

            cout << "h = ";
            cin >> h;

            clearInput();

            auto f = [&](double value) {

                string replaced;

                for (char c : expression) {

                    if (c == 'x') {

                        ostringstream oss;
                        oss << setprecision(15)
                            << value;

                        replaced += "(" + oss.str() + ")";
                    }
                    else {
                        replaced += c;
                    }
                }

                return evaluate(replaced);
            };

            try {

                double derivative =
                    (f(x + h) - f(x - h)) /
                    (2 * h);

                cout << "\nf'(" << x << ") = "
                     << derivative << "\n";

            }
            catch (const exception& ex) {

                cout << "Error: "
                     << ex.what() << "\n";
            }

            pauseScreen();
        }

        else if (choice == 2) {

            string expression;

            double a, b;
            int n;

            cout << "\nEnter f(x).\n";
            cout << "Example: x^2\n\n";

            cout << "f(x) = ";
            getline(cin, expression);

            cout << "Lower limit = ";
            cin >> a;

            cout << "Upper limit = ";
            cin >> b;

            cout << "Number of intervals (even) = ";
            cin >> n;

            clearInput();

            if (n <= 0 || n % 2 != 0) {

                cout << "Number of intervals must be positive and even.\n";
                pauseScreen();
                continue;
            }

            auto f = [&](double value) {

                string replaced;

                for (char c : expression) {

                    if (c == 'x') {

                        ostringstream oss;
                        oss << setprecision(15)
                            << value;

                        replaced += "(" + oss.str() + ")";
                    }
                    else {
                        replaced += c;
                    }
                }

                return evaluate(replaced);
            };

            try {

                double h =
                    (b - a) / n;

                double sum =
                    f(a) + f(b);

                for (int i = 1; i < n; ++i) {

                    double x =
                        a + i * h;

                    if (i % 2 == 0)
                        sum += 2 * f(x);
                    else
                        sum += 4 * f(x);
                }

                double result =
                    sum * h / 3.0;

                cout << "\nIntegral = "
                     << result << "\n";
            }

            catch (const exception& ex) {

                cout << "Error: "
                     << ex.what() << "\n";
            }

            pauseScreen();
        }
    }
}

// ------------------------------------------------------------
// Base-N Calculator
// ------------------------------------------------------------

long long toDecimal(const string& value, int base) {

    long long result = 0;

    for (char c : value) {

        int digit;

        if (c >= '0' && c <= '9')
            digit = c - '0';

        else if (c >= 'A' && c <= 'F')
            digit = c - 'A' + 10;

        else if (c >= 'a' && c <= 'f')
            digit = c - 'a' + 10;

        else
            throw runtime_error("Invalid digit.");

        if (digit >= base)
            throw runtime_error("Digit outside base.");

        result = result * base + digit;
    }

    return result;
}

string fromDecimal(long long value, int base) {

    if (value == 0)
        return "0";

    bool negative = value < 0;

    if (negative)
        value = -value;

    string digits = "0123456789ABCDEF";
    string result;

    while (value > 0) {

        result += digits[value % base];
        value /= base;
    }

    reverse(result.begin(), result.end());

    if (negative)
        result = "-" + result;

    return result;
}

void baseMode() {

    while (true) {

        header("BASE-N CALCULATOR");

        cout << "1. Convert number\n";
        cout << "2. Bitwise AND\n";
        cout << "3. Bitwise OR\n";
        cout << "4. Bitwise XOR\n";
        cout << "5. Back\n\n";

        cout << "Select: ";

        int choice;

        if (!(cin >> choice)) {
            clearInput();
            continue;
        }

        clearInput();

        if (choice == 5)
            return;

        if (choice == 1) {

            int baseFrom, baseTo;
            string value;

            cout << "Input base (2-16): ";
            cin >> baseFrom;

            cout << "Number: ";
            cin >> value;

            cout << "Output base (2-16): ";
            cin >> baseTo;

            clearInput();

            try {

                long long decimal =
                    toDecimal(value, baseFrom);

                cout << "\nResult = "
                     << fromDecimal(decimal, baseTo)
                     << "\n";

            }
            catch (const exception& ex) {

                cout << "Error: "
                     << ex.what() << "\n";
            }

            pauseScreen();
        }

        else if (choice >= 2 && choice <= 4) {

            int base;

            string a, b;

            cout << "Base (2-16): ";
            cin >> base;

            cout << "A: ";
            cin >> a;

            cout << "B: ";
            cin >> b;

            clearInput();

            try {

                long long x =
                    toDecimal(a, base);

                long long y =
                    toDecimal(b, base);

                long long result;

                if (choice == 2)
                    result = x & y;

                else if (choice == 3)
                    result = x | y;

                else
                    result = x ^ y;

                cout << "\nResult = "
                     << fromDecimal(result, base)
                     << "\n";

            }
            catch (const exception& ex) {

                cout << "Error: "
                     << ex.what() << "\n";
            }

            pauseScreen();
        }
    }
}

// ------------------------------------------------------------
// Main Menu
// ------------------------------------------------------------

void mainMenu() {

    while (true) {

        header("RICARDIAN CALCULATOR");

        cout << "Mode: " << angleName() << "\n\n";

        cout << "  1. COMP       Scientific Calculator\n";
        cout << "  2. CMPLX      Complex Numbers\n";
        cout << "  3. STAT       Statistics\n";
        cout << "  4. MATRIX     Matrix Calculator\n";
        cout << "  5. VECTOR     Vector Calculator\n";
        cout << "  6. EQN        Equation Solver\n";
        cout << "  7. CALC       Numerical Calculus\n";
        cout << "  8. BASE-N     Number Bases\n";
        cout << "  9. EXIT\n\n";

        cout << "Select mode: ";

        int choice;

        if (!(cin >> choice)) {

            clearInput();

            cout << "\nPlease enter a number.\n";
            continue;
        }

        clearInput();

        switch (choice) {

            case 1:
                scientificMode();
                break;

            case 2:
                complexMode();
                break;

            case 3:
                statisticsMode();
                break;

            case 4:
                matrixMode();
                break;

            case 5:
                vectorMode();
                break;

            case 6:
                equationMode();
                break;

            case 7:
                calculusMode();
                break;

            case 8:
                baseMode();
                break;

            case 9:

                cout << "\nThank you for using the Ricardian Calculator!\n";
                return;

            default:

                cout << "\nInvalid selection.\n";
                break;
        }
    }
}

// ------------------------------------------------------------
// Program Start
// ------------------------------------------------------------

int main() {

    srand(static_cast<unsigned>(time(nullptr)));

    cout << fixed << setprecision(10);

    // Important: this guarantees that the program
    // displays something immediately.

    cout << "\n";
    cout << "============================================================\n";
    cout << "             RICARDIAN CALCULATOR\n";
    cout << "============================================================\n";
    cout << "\n";
    cout << "Starting calculator...\n";

    mainMenu();

    return 0;
}