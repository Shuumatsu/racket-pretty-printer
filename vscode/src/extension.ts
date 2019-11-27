import * as vscode from 'vscode'
import { spawnSync } from 'child_process'

export const command = 'racket-pretty-printer'

const getFullRange = (document: vscode.TextDocument) => {
    const firstLine = document.lineAt(0)
    const lastLine = document.lineAt(document.lineCount - 1)
    return new vscode.Range(0, firstLine.range.start.character, document.lineCount - 1, lastLine.range.end.character)
}

export function activate(context: vscode.ExtensionContext) {
    console.log('Congratulations, your extension "racket-pretty-printer" is now active!')

    context.subscriptions.push(
        vscode.commands.registerCommand('extension.racket-pretty', () => {
            const { activeTextEditor } = vscode.window

            if (activeTextEditor && activeTextEditor.document.languageId === 'foo-lang') {
                const { document } = activeTextEditor
                const text = document.getText()
                const { stdout, stderr } = spawnSync(command, ['--stdin'], { input: text, encoding: 'utf8' })

                const edit = new vscode.WorkspaceEdit()
                const range = getFullRange(document)
                edit.replace(document.uri, range, stdout)
                return vscode.workspace.applyEdit(edit)
            }
        })
    )

    const formatter = vscode.languages.registerDocumentFormattingEditProvider('racket', {
        provideDocumentFormattingEdits: (
            document: vscode.TextDocument,
            options: vscode.FormattingOptions
        ): vscode.ProviderResult<vscode.TextEdit[]> =>
            new Promise((resolve, reject) => {
                const text = document.getText()
                const { stdout, stderr } = spawnSync(command, ['--stdin'], { input: text, encoding: 'utf8' })
                if (stderr) return reject(stderr)

                const range = getFullRange(document)
                return resolve([vscode.TextEdit.replace(range, stdout)])
            })
    })
    context.subscriptions.push(formatter)
}

export function deactivate() {}
