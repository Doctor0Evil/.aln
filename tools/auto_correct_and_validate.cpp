// =============================================================
// C++ Repo Auto-Correct & Layout Validator Tool
// File: auto_correct_and_validate.cpp
// Description:
//   - Scans project root for required and obsolete directories.
//   - Auto-corrects all *.aln files: converts Windows line endings to LF,
//     trims trailing whitespace from each line.
//   - Reports compliance status to stdout. Returns nonzero exit code on error.
// =============================================================

#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <filesystem>
#include <regex>

namespace fs = std::filesystem;

// --------- Configurable Sections ---------
const std::vector<std::string> required_dirs = {
    ".github", ".github/workflows", "scripts", "Modules/ALNCore"
};
const std::vector<std::string> obsolete_dirs = {
    "OldDocs", "backup", "tmp", "__old", ".trash", "legacy_code"
};

// --------- Utility Functions -------------
bool exists(const std::string& path) {
    return fs::exists(path) && fs::is_directory(path);
}

void error_and_exit(const std::string& msg, int code = 1) {
    std::cerr << "❗ " << msg << std::endl;
    exit(code);
}

void check_required_dirs() {
    for (const auto& dir : required_dirs) {
        if (!exists(dir)) {
            error_and_exit("Missing directory: " + dir);
        }
    }
    std::cout << "✅ All critical directories are present." << std::endl;
}

void check_obsolete_dirs() {
    for (const auto& dir : obsolete_dirs) {
        if (exists(dir)) {
            error_and_exit("Obsolete directory present: " + dir);
        }
    }
    std::cout << "✅ No obsolete directories detected." << std::endl;
}

void auto_correct_aln(const fs::path& file) {
    std::ifstream in(file, std::ios::binary);
    std::string content((std::istreambuf_iterator<char>(in)), std::istreambuf_iterator<char>());
    in.close();

    // Convert CRLF to LF
    content = std::regex_replace(content, std::regex("\r\n"), "\n");

    // Trim trailing whitespace from each line
    std::istringstream ss(content);
    std::string line, new_content;
    while (std::getline(ss, line)) {
        line = std::regex_replace(line, std::regex("[ \t]+$"), "");
        new_content += line + "\n";
    }

    std::ofstream out(file, std::ios::binary | std::ios::trunc);
    out << new_content;
    out.close();

    std::cout << "Corrected: " << file << std::endl;
}

void validate_aln_structure(const fs::path& file) {
    std::ifstream in(file);
    std::string first_line;
    std::getline(in, first_line);
    in.close();
    if (first_line.find("ALN_WORKFLOW {") == std::string::npos) {
        error_and_exit("ALN file missing workflow block: " + file.string());
    }
}

void process_aln_files() {
    for (auto& p : fs::recursive_directory_iterator(".")) {
        if (p.is_regular_file() && p.path().extension() == ".aln") {
            auto_correct_aln(p.path());
            validate_aln_structure(p.path());
        }
    }
    std::cout << "✅ All .aln files auto-corrected & structurally correct." << std::endl;
}

// --------- Main Entrypoint ---------------
int main() {
    try {
        check_required_dirs();
        check_obsolete_dirs();
        process_aln_files();
        std::cout << "[COMPLETE] Repo directory and ALN file compliance enforced." << std::endl;
        return 0;
    } catch (...) {
        error_and_exit("Internal error during validation/correction.");
    }
}
