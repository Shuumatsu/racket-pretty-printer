import * as vscode from 'vscode'
import { spawnSync } from 'child_process'

export const command = 'racket-pretty-printer'

const getFullRange = (document: vscode.TextDocument) => {
    const firstLine = document.lineAt(0)
    const lastLine = document.lineAt(document.lineCount - 1)
    return new vscode.Range(0, firstLine.range.start.character, document.lineCount - 1, lastLine.range.end.character)
}

const format = (text: string) => {
    const config = vscode.workspace.getConfiguration('racket-pretty-printer')
    const args = ['--stdin']
    config.has('width') && args.push('--width', `${config.get('width')}`)
    config.has('ribbon') && args.push('--ribbon', `${config.get('ribbon')}`)
    console.log(args)
    return spawnSync(command, args, { input: text, encoding: 'utf8' })
}

export function activate(context: vscode.ExtensionContext) {
    console.log('Congratulations, your extension "racket-pretty-printer" is now active!')

    context.subscriptions.push(
        vscode.commands.registerCommand('extension.racket-pretty', () => {
            const { activeTextEditor } = vscode.window

            if (!activeTextEditor) return

            const { document } = activeTextEditor
            const text = document.getText()
            const { stderr, stdout } = format(text)
            if (stderr) return console.log('err', stderr)

            const edit = new vscode.WorkspaceEdit()
            const range = getFullRange(document)
            edit.replace(document.uri, range, stdout)
            console.log(text, stdout)
            return vscode.workspace.applyEdit(edit)
        })
    )

    const formatter = vscode.languages.registerDocumentFormattingEditProvider(
        { pattern: '**/*.rkt' },
        {
            provideDocumentFormattingEdits: (
                document: vscode.TextDocument,
                options: vscode.FormattingOptions
            ): vscode.ProviderResult<vscode.TextEdit[]> =>
                new Promise((resolve, reject) => {
                    const text = document.getText()
                    const { stderr, stdout } = format(text)
                    if (stderr) return reject(stderr)

                    const range = getFullRange(document)
                    return resolve([vscode.TextEdit.replace(range, stdout)])
                })
        }
    )
    context.subscriptions.push(formatter)
}

export function deactivate() {}
